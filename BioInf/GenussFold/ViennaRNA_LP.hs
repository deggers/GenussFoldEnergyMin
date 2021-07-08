{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module BioInf.GenussFold.ViennaRNA_LP where

import           Debug.Trace

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower,isSpace)
import           Data.List
import           Data.Maybe
import           Data.Vector.Fusion.Util
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import           Text.Printf

import           ADP.Fusion
import           Data.PrimitiveArray as PA hiding (map)

import           FormalLanguage

import           BioInf.GenussFold.IndexToIndexParser
import           BioInf.GenussFold.IdxStrng
import qualified BioInf.ViennaRNA.Bindings.Inline as V
import qualified Data.ByteString.Char8 as BS
import           GHC.Float

import           BioInf.GenussFold.Grammars.ViennaRNA_LP

-- Use domain-specific language
type Pos = Int
type Nt = Char
type NtPos = (Nt, Pos)
type Basepair = (Nt, Nt)
type Energy = Int
type IndexRegionParser = (Pos,Pos)
ignore      = 100123

-- Used as energies for the psuedoknot extension
interiorLoopEnergy ::  Basepair -> Basepair -> Energy
interiorLoopEnergy ('C','G') ('G','U') = -140
interiorLoopEnergy ('G','U') ('C','G') = -250
interiorLoopEnergy ('C','G') ('U','A') = -210
interiorLoopEnergy ('U','A') ('U','A') = -090
interiorLoopEnergy ('C','G') ('A','U') = -210
interiorLoopEnergy ('A','U') ('U','A') = -110
interiorLoopEnergy ('U','A') ('A','U') = -130
interiorLoopEnergy ('U','A') ('C','G') = -240
interiorLoopEnergy ('C','G') ('C','G') = -330
interiorLoopEnergy ('C','G') ('U','G') = -210
interiorLoopEnergy ('U','G') ('A','U') = -100
interiorLoopEnergy ('A','U') ('A','U') = -090
interiorLoopEnergy ('G','C') ('A','U') = -240
interiorLoopEnergy ('A','U') ('G','C') = -210
interiorLoopEnergy _ _ = -151

energyMinAlg :: Monad m => BS.ByteString ->  SigEnergyMin m Energy Energy NtPos (Pos,Pos)
energyMinAlg input = SigEnergyMin
  { nil  = \ () -> 0
  , pkn = \ x y -> x + y
  , hpk = \ () () x y
    -> let m = maximum [x,y] in if m < 0 then x + y -500 else ignore -- @TODO FIX Penalty
    -- check for indicis again because of extension .. draw as picture to visualize it because different to "normal stacks"
    -- nicht von kleiner zu größer
   , pk1 = \ (Z:.():.(lPos,jPos)) (Z:.():.m) y (Z:.(subtract 1 -> iPos, subtract 1 -> kPos):.()) (Z:.n:.()) -> if -- @TODO which indexes must be increment or decrementd for vienna and first band i k, second l j
       -- interiorLoops instead of stacking in LP+
       | (minimum [lPos,jPos,iPos,kPos] >= 0) && (jPos < BS.length input) && pairs (BS.index input iPos) (BS.index input jPos) && pairs (BS.index input kPos) (BS.index input lPos)
         -- -> traceShow ("pk1" ++ show (iPos,kPos,lPos,jPos)) $ m + n + y - evalIP input iPos kPos lPos jPos
         -> m + n + y - 330
       | otherwise -> ignore
  , pk2 = \ (Z:.():.(lPos,subtract 1 -> jPos)) (Z:.():.m) y (Z:.(subtract 1 -> iPos, subtract 1 -> kPos):.()) (Z:.n:.()) -> if -- @TODO which indexes must be increment or decrementd for vienna and first band i k, second l j
      | (minimum [lPos,jPos,iPos,kPos] >= 0) && pairs (BS.index input iPos) (BS.index input jPos) && pairs (BS.index input kPos) (BS.index input lPos)
         -> m + n + y - 330
        -- -> traceShow ("pk2" ++ show (iPos,kPos,lPos,jPos)) $ m + n + y - 330
      | otherwise -> ignore
  , pk1b = \ (Z:.(i,iPos):.()) (Z:.():.(j,jPos)) (Z:.s1:.()) (Z:.():.s2) -> if
      | pairs i j
        -> s1 + s2 - 330
      --  -> traceShow ("pk1b" ++ show(i,iPos,j,jPos)) $ s1 + s2 - 330
      | otherwise -> ignore -- @TODO Fix simple +1
  , pk2b = \ (Z:.(i,iPos):.()) (Z:.():.(j,jPos)) (Z:.s1:.()) (Z:.():.s2) -> if
      | pairs i j
        -- ->  traceShow ("pk2b" ++ show(i,iPos,j,jPos)) $ s1 + s2 - 330
        ->  s1 + s2 - 330
      | otherwise -> ignore -- @TODO Fix simple +1
  , unp = \ c ss ->  ss
  , jux   = \ x y -> x + y -- traceShow ("JXP" ++ show (x,y)) $ x + y
  , hairpin  = \ (iPos, subtract 1 -> jPos) -> if
             | (jPos-iPos) > 3 && pairs (BS.index input iPos) (BS.index input jPos)
               -> (evalHP input iPos jPos) - 1500
               -- -> traceShow ("HP" ++ show (iPos,jPos, evalHP input iPos jPos)) $ evalHP input iPos jPos
             | otherwise -> ignore

  , interior = \ (iPos, kPos) closed (subtract 1 -> lPos, subtract 1 -> jPos) -> let e = evalIP input iPos jPos kPos lPos in if
             | pairs (BS.index input iPos) (BS.index input jPos)
               && pairs (BS.index input kPos) (BS.index input lPos)
               -> closed + evalIP input iPos jPos kPos lPos
               -- -> traceShow ("INT " ++ show (closed,iPos,jPos,kPos,lPos,e)) $ e + closed -- evalIP input iPos jPos kPos lPos  --subtract 330 closed -- interiorLoopEnergy (BS.index input iPos, BS.index input jPos) (BS.index input kPos, BS.index input lPos)
             | otherwise -> ignore

  , mlr      = \ (a,_) m m1 (d,_) -> if
             | pairs a d -> m + m1 + 290
             | otherwise   -> ignore

  , mcm_1 = \ _ closed -> closed
  , mcm_2 = \  m closed ->  m + closed
  , mcm_3 = \  m _ ->  m
  , ocm_1 = \  m1 -> m1
  , ocm_2 = \ closed _ -> closed
  , h    =   SM.foldl' min (ignore)
  }
{-# INLINE energyMinAlg #-}

prettyPaths :: Monad m => BS.ByteString -> SigEnergyMin m [String] [[String]] NtPos (Pos,Pos)
prettyPaths input = SigEnergyMin
  { nil = \ () ->  [""]
  , pkn = \ [x] [y] -> [x ++ y]
  , hpk = \ () () [x1,x2] [y1,y2]
    -> [x1 ++ y1 ++ x2 ++ y2]
, pk1 = \ (Z:.():.(lPos,jPos)) (Z:.():.[t2]) [x1,x2] (Z:.(subtract 1 -> iPos, subtract 1 -> kPos):.()) (Z:.[t1]:.()) --  @TODO FIX must know if interior
    -> [ x1 ++ "pk1_l with (" ++ show (iPos,kPos) ++ ") " ++ t1 , "pk1_r with ("++ show (lPos,jPos)++ ") " ++  t2 ++ x2]
  , pk1b = \ (Z:.(iPos):.()) (Z:.():.(jPos)) (Z:.[t1]:.()) (Z:.():.[t2])
    -> ["(pk1b_l with " ++ show iPos ++ ") " ++ t1 , "pk1b_r with " ++ show jPos  ++") " ++ t2]
  , pk2 = \ _ (Z:.():.[s2]) [x1,x2] _ (Z:.[s1]:.()) -- @TODO FIX must handle interior
    -> [ x1 ++ "[" ++ s1 , "]" ++  s2 ++ x2]
  , pk2b = \ _ _ (Z:.[t1]:.()) (Z:.():.[t2])
    -> ["[" ++ t1 , "]" ++ t2]
  , unp = \  _ [ss] -> [ss]
  , jux = \ [x] [y] -> [x ++ y]
  , hairpin = \  (iPos, subtract 1 -> jPos)  ->
      ["Hairpin Loop (" ++ show iPos ++ "," ++ show jPos ++ "): " ++ show (V.hairpinP input iPos jPos)]
  , interior = \ (i,k) [closed] (subtract 1 -> l, subtract 1 -> j) ->
      ["Interior loop (" ++ show i ++ "," ++ show k ++ ") (" ++ show l ++ "," ++ show j ++  "):" ++ show (evalIP input i j k l) ++ " " ++ closed]
  , mlr = \ a [m] [m1] b ->
      ["Multi (" ++ show a ++ "," ++ show b ++ ") m:" ++ m ++ " m1: " ++ m1 ++ " Multi (?,?) "]
  , mcm_1 = \ _ [closed] -> [closed]
  , mcm_2 = \ [m] [closed] -> [m ++ closed]
  , mcm_3 = \ [m] _ -> [m]
  , ocm_1 = \ [m1] -> [m1]
  , ocm_2 = \ [x] _ -> [x]
  , h   = SM.toList
  }
{-# INLINE prettyPaths #-}

pretty :: Monad m => SigEnergyMin m [String] [[String]] NtPos (Pos,Pos)
pretty = SigEnergyMin
  { nil = \ () ->  [""]
  , pkn = \ [x] [y] -> [x ++ y]
  , hpk = \ () () [x1,x2] [y1,y2]
    -> [x1 ++ y1 ++ x2 ++ y2]
  , pk1 = \ (Z:.():.(lPos,jPos)) (Z:.():.[t2]) [x1,x2] (Z:.(iPos,kPos):.()) (Z:.[t1]:.())
    -> [ x1 ++ replicate (kPos-iPos-1) '.' ++ "{" ++ t1 , "}" ++ replicate (jPos-lPos-1) '.' ++ t2 ++ x2]
  , pk2 = \ (Z:.():.(lPos,jPos)) (Z:.():.[t2]) [x1,x2] (Z:.(iPos,kPos):.()) (Z:.[t1]:.())
    -> [ x1 ++ replicate (kPos-iPos-1) '.' ++ "[" ++ t1 , "]" ++ replicate (jPos-lPos-1) '.' ++ t2 ++ x2]
  , pk1b = \ (Z:.(l,lPos):.()) (Z:.():.(r,rPos)) (Z:.[t1]:.()) (Z:.():.[t2])
    -> ["{" ++ t1 , "}" ++ t2]
  , pk2b = \ (Z:.(l,lPos):.()) (Z:.():.(r,rPos)) (Z:.[t1]:.()) (Z:.():.[t2])
    -> ["[" ++ t1 , "]" ++ t2]
  , unp = \ _ [ss] -> ["." ++ ss]
  , jux = \ [x] [y] -> [x ++ y]
  , hairpin = \  (iPos,subtract 1 -> jPos)  -> ["(" ++ replicate (jPos-iPos-1) '.' ++ ")"]
  , interior = \ (iPos, kPos) [closed] (subtract 1 -> lPos, subtract 1 -> jPos)
    -> ["(" ++ replicate (kPos-iPos-1) '.' ++ closed ++ replicate (jPos-lPos-1) '.' ++ ")"]
  , mlr = \ _ [m] [m1] _ -> ["(" ++ m ++ m1 ++ ")"]
  , mcm_1 = \ (iPos,  jPos) [closed] -> [replicate (jPos-iPos) '.' ++ closed ]
  , mcm_2 = \ [m] [closed] -> [m ++ closed ]
  , mcm_3 = \ [m] _ -> [m ++ "."]
  , ocm_1 = \ [m1] -> [m1]
  , ocm_2 = \ [x] _ -> [x ++ "."]
  , h   = SM.toList
  }
{-# INLINE pretty #-}

energyMin :: Int -> String -> (Energy,[[String]])
energyMin k inp = (z, take k bs) where
  i = BS.pack . Prelude.map toUpper $ inp
  iv = VU.fromList . Prelude.map toUpper $ inp
  !(Z:.a:.b:.e:.f:.g:.j:.m) = runInsideForward i iv
  z = unId $ axiom a -- gets the value from the table
  bs = runInsideBacktrack i iv (Z:.a:.b:.e:.f:.g:.j:.m)
{-# NOINLINE energyMin #-}

type X = ITbl Id Unboxed Subword Energy
type T = ITbl Id Unboxed (Z:.Subword:.Subword) Energy

runInsideForward :: BS.ByteString -> VU.Vector Char -> Z:.X:.X:.X:.X:.X:.T:.T
runInsideForward i iv = mutateTablesWithHints (Proxy :: Proxy MonotoneMCFG)
                   $ gEnergyMin (energyMinAlg i)
                        (ITbl 0 1 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (166999) []))
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (266999) []))
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (366999) []))
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (466999) []))
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (566999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (777999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (888999) []))
                        (ntPos iv )
                        (idxStrng1 1 31 iv)
  where n = BS.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: BS.ByteString -> VU.Vector Char -> Z:.X:.X:.X:.X:.X:.T:.T -> [[String]] -- for the non-terminals
runInsideBacktrack i iv  (Z:.a:.b:.e:.f:.j:.k:.q) = unId $ axiom g -- Axiom from the Start Nonterminal S -> a_Struct-
  where !(Z:.g:._:._:._:._:._:._) = gEnergyMin (energyMinAlg i <|| pretty)
  -- where !(Z:.g:.h:.l:.m:.n:.o:.p) = gEnergyMin (energyMinAlg i <|| prettyPaths i)
                          (toBacktrack a (undefined :: Id y -> Id y))
                          (toBacktrack b (undefined :: Id y -> Id y))
                          (toBacktrack e (undefined :: Id y -> Id y))
                          (toBacktrack f (undefined :: Id y -> Id y))
                          (toBacktrack j (undefined :: Id y -> Id y))
                          (toBacktrack k (undefined :: Id y -> Id y))
                          (toBacktrack q (undefined :: Id y -> Id y))
                          (ntPos iv)
                          (idxStrng1 1 31 iv)
{-# NoInline runInsideBacktrack #-}

-- @TODO remember to activate AU and GU base pairs!
pairs :: Char -> Char -> Bool
pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
--  || c=='G' && d=='U'
  || c=='U' && d=='A'
--  || c=='U' && d=='G'
{-# INLINE pairs #-}


ntPos :: VG.Vector v x => v x -> Chr (x,Int) x
{-# Inline ntPos #-}
ntPos xs = Chr f xs where
  {-# Inline [0] f #-}
  f xs k = (VG.unsafeIndex xs k, k)

-- | Until i figure out how to fix the addition of 1 in the lib
evalIP :: BS.ByteString -> Int -> Int -> Int -> Int -> Energy
evalIP input ((+1 ) -> i) ( (+1) -> j) ((+1) -> k) ((+1) -> l) = V.intLoopP input i j k l

evalHP :: BS.ByteString -> Int -> Int -> Energy
evalHP input ((+1) -> i) ((+1) -> j) = V.hairpinP input i j

evalMB :: BS.ByteString -> Int -> Int -> Energy
evalMB input ((+1) -> i) ((+1) -> j) = V.mbLoopP input i j

{--
Look at:
https://hackage.haskell.org/package/RNAFold-1.99.3.4/docs/src/BioInf-ViennaRNA-Fold.html

Teststructures :

Multiloop Structure
  External loop                           :  -110
  Interior loop (  2, 41) CG; (  3, 40) CG:  -330
  Interior loop (  3, 40) CG; (  4, 39) CG:  -330
  Interior loop (  4, 39) CG; (  5, 38) CG:  -330
  Interior loop (  7, 19) CG; (  8, 18) CG:  -330
  Interior loop (  8, 18) CG; (  9, 17) CG:  -330
  Interior loop (  9, 17) CG; ( 10, 16) CG:  -330
  Hairpin  loop ( 10, 16) CG              :   420
  Interior loop ( 22, 36) CG; ( 23, 35) CG:  -330
  Interior loop ( 23, 35) CG; ( 24, 34) CG:  -330
  Interior loop ( 24, 34) CG; ( 25, 33) CG:  -330
  Interior loop ( 25, 33) CG; ( 26, 32) CG:  -330
  Hairpin  loop ( 26, 32) CG              :   420
  Multi    loop (  5, 38) CG              :   290
  ACCCCACCCCAAAAAGGGGAACCCCCAAAAAGGGGGAGGGGA
  .((((.((((.....))))..(((((.....))))).)))). (-22.80)

:l BioInf/GenussFold/ViennaRNA.hs
let seq = BS.pack "ACCCCACCCCAAAAAGGGGAACCCCCAAAAAGGGGGAGGGGA"
-}
