{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module BioInf.GenussFold.ViennaRNA where

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

import           BioInf.GenussFold.Grammars.ViennaRNA

-- Use domain-specific language
type Pos = Int
type Nt = Char
type NtPos = (Nt, Pos)
type Basepair = (Nt, Nt)
type Energy = Int
type IndexRegionParser = (Pos,Pos)
ignore      = 100123

energyMinAlg :: Monad m => BS.ByteString ->  SigEnergyMin m Energy Energy NtPos (Pos,Pos)
energyMinAlg input = SigEnergyMin
  { nil  = \ () -> 0
  , unpaired = \ _ ss -> ss
  , juxtaposed   = \ x y -> x + y -- traceShow ("JXP" ++ show (x,y)) $ x + y
  , hairpin  = \ (iPos, subtract 1 -> jPos) -> if
             | (jPos-iPos) > 3 && pairs (BS.index input iPos) (BS.index input jPos)
               -> evalHP input iPos jPos
               -- -> traceShow ("HP" ++ show (iPos,jPos, evalHP input iPos jPos)) $ evalHP input iPos jPos
             | otherwise -> ignore

  , interior = \ (iPos, kPos) closed (subtract 1 -> lPos, subtract 1 -> jPos) -> let e = evalIP input iPos jPos kPos lPos in if
             | pairs (BS.index input iPos) (BS.index input jPos)
               && pairs (BS.index input kPos) (BS.index input lPos)
               -> closed + evalIP input iPos jPos kPos lPos
               -- -> traceShow ("INT " ++ show (closed,iPos,jPos,kPos,lPos,e)) $ e + closed -- evalIP input iPos jPos kPos lPos  --subtract 330 closed -- interiorLoopEnergy (BS.index input iPos, BS.index input jPos) (BS.index input kPos, BS.index input lPos)
             | otherwise -> ignore

  , mlr      = \ (a,iPos) m m1 (d,jPos) -> if
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
  , unpaired = \  _ [ss] -> [ss]
  , juxtaposed = \ [x] [y] -> [x ++ y]
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
  , unpaired = \ _ [ss] -> ["." ++ ss]
  , juxtaposed = \ [x] [y] -> [x ++ y]
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
  !(Z:.a:.b:.e:.f) = runInsideForward i iv
  z = unId $ axiom a -- gets the value from the table
  bs = runInsideBacktrack i iv (Z:.a:.b:.e:.f)
{-# NOINLINE energyMin #-}

type X = ITbl Id Unboxed Subword Energy
-- PK will need subwords over subwords

runInsideForward :: BS.ByteString -> VU.Vector Char -> Z:.X:.X:.X:.X
runInsideForward i iv = mutateTablesWithHints (Proxy :: Proxy CFG)
                   $ gEnergyMin (energyMinAlg i)
                        (ITbl 0 1 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (166999) []))
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (266999) []))
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (366999) []))
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (466999) []))
                        (ntPos iv )
                        (idxStrng1 1 31 iv)
  where n = BS.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: BS.ByteString -> VU.Vector Char -> Z:.X:.X:.X:.X -> [[String]] -- for the non-terminals
runInsideBacktrack i iv  (Z:.a:.b:.e:.f) = unId $ axiom g -- Axiom from the Start Nonterminal S -> a_Struct-
  where !(Z:.g:.h:.l:.m) = gEnergyMin (energyMinAlg i <|| pretty)
  -- where !(Z:.g:.h:.l:.m) = gEnergyMin (energyMinAlg i <|| prettyPaths i)
                          (toBacktrack a (undefined :: Id a -> Id a))
                          (toBacktrack b (undefined :: Id b -> Id b))
                          (toBacktrack e (undefined :: Id e -> Id e))
                          (toBacktrack f (undefined :: Id f -> Id f))
                          (ntPos iv)
                          (idxStrng 1 31 iv)
{-# NoInline runInsideBacktrack #-}

pairs :: Char -> Char -> Bool
pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
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
