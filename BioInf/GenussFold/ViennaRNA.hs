{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}

{-# Options_GHC -fspec-constr-count=1000 #-}
{-# Options_GHC -fspec-constr-recursive=1000 #-}
{-# Options_GHC -fspec-constr-threshold=1000 #-}
{-# Options_GHC -fmax-worker-args=1000 #-} -- temporäre funktionen bis zu 1000 args ok
{-# Options_GHC -flate-dmd-anal #-} -- gibts wahrscheinlich nicht in ghci 7 - late demand analys
-- both, full laziness and no liberate case are essential to have things inline nicely!
{-# Options_GHC -fno-full-laziness #-} -- stellt sicher das keine argument rausgeworfen werden könnte probleme mit Fusion erzeugen
{-# Options_GHC -fno-liberate-case #-} -- alle cases betrachten

module BioInf.GenussFold.ViennaRNA where

import           Debug.Trace
import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Control.DeepSeq
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
import           Foreign.Ptr
import           System.IO.Unsafe

import           ADP.Fusion
import           Data.PrimitiveArray as PA hiding (map)

import           FormalLanguage

import           BioInf.GenussFold.IndexToIndexParser
import           BioInf.GenussFold.IdxStrng
import qualified BioInf.ViennaRNA.Bindings.Inline as V
import qualified Data.ByteString.Char8 as BS
import           GHC.Float

import           BioInf.GenussFold.Grammars.ViennaRNA

-- Use +RTS -s    for some runtime stuff
-- echo "CCGAGCCCCCCCCGGCAGG" | ./ViennaRNA_LP -p 700 +RTS -s

-- Use domain-specific language
type Pos = Int
type Nt = Char
type NtPos = (Nt, Pos)
type Basepair = (Nt, Nt)
type Energy = Int
type IndexRegionParser = (Pos,Pos)
ignore      = 100123

-- @NOTE :: Penalty only to make looping over all algorithms easier - is not applied here
energyMinAlg :: Monad m => BS.ByteString -> Int -> Ptr() -> SigEnergyMin m Energy Energy NtPos (Pos,Pos)
energyMinAlg input penalty compound = SigEnergyMin
  { nil  = \ () -> 0
  , unp = \ c ss ->  ss
  , jux   = \ x y -> x + y -- traceShow ("JXP" ++ show (x,y)) $ x + y
  , hairpin  = \ (iPos, subtract 1 -> jPos) -> if
    | (jPos-iPos) > 3 && pairs (BS.index input iPos) (BS.index input jPos)
      -> (evalHP compound input iPos jPos)
  -- -> traceShow ("HP" ++ show (iPos,jPos, evalHP input iPos jPos)) $ evalHP input iPos jPos
    | otherwise -> ignore
  , interior = \ (iPos, kPos) closed (subtract 1 -> lPos, subtract 1 -> jPos) -> let e = evalIP compound input iPos jPos kPos lPos in if
    | pairs (BS.index input iPos) (BS.index input jPos)
      && pairs (BS.index input kPos) (BS.index input lPos)
      -> closed + evalIP compound input iPos jPos kPos lPos
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

pretty :: Monad m => SigEnergyMin m [String] [[String]] NtPos (Pos,Pos)
pretty = SigEnergyMin
  { nil = \ () ->  [""]
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

-- The number of desired backtracks
newtype NumBT = NumBT Int -- alias for an Int :: But newtype as explicit naming
  deriving (Num, Show, Read)

-- The penalty for opening a pseudoknot
newtype PenPK = PenPK Int
  deriving Num

instance Read PenPK where
 readsPrec p s = [ (PenPK i, t) | (i,t) <- readsPrec p s ]

instance Show PenPK where
  show (PenPK p) = show p

energyMin :: NumBT -> PenPK -> String -> (Energy,[[String]])
energyMin (NumBT k) (PenPK p) inp = unsafePerformIO $ do
  let
    i = BS.pack . Prelude.map toUpper $ inp
  c <- V.mkFoldCompound i
  let
    iv = VU.fromList . Prelude.map toUpper $ inp
    !(Z:.a:.b:.e:.f) = runInsideForward i iv p c
    z = unId $ axiom a -- gets the value from the table
    bs = take k $ runInsideBacktrack i iv p c (Z:.a:.b:.e:.f) 
  deepseq bs $ V.destroyFoldCompound c -- deepseq so thats fully evaluated before continue
  return (z, bs)
{-# NOINLINE energyMin #-}

type X = ITbl Id Unboxed Subword Energy

runInsideForward :: BS.ByteString -> VU.Vector Char -> Int -> Ptr() -> Z:.X:.X:.X:.X
runInsideForward i iv p c = mutateTablesWithHints (Proxy :: Proxy CFG)
                   $ gEnergyMin (energyMinAlg i p c)
                        (ITbl 0 1 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (166999) []))
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (266999) []))
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (366999) []))
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (466999) []))
                        (ntPos iv )
                        (idxStrng1 1 31 iv)
  where n = BS.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: BS.ByteString -> VU.Vector Char -> Int -> Ptr() -> Z:.X:.X:.X:.X -> [[String]] -- for the non-terminals
runInsideBacktrack i iv p c (Z:.a:.b:.e:.f) = unId $ axiom g -- Axiom from the Start Nonterminal S -> a_Struct-
  where !(Z:.g:._:._:._) = gEnergyMin (energyMinAlg i p c<|| pretty)
  -- where !(Z:.g:.h:.l:.m:.n:.o:.p) = gEnergyMin (energyMinAlg i <|| prettyPaths i)
                          (toBacktrack a (undefined :: Id y -> Id y))
                          (toBacktrack b (undefined :: Id y -> Id y))
                          (toBacktrack e (undefined :: Id y -> Id y))
                          (toBacktrack f (undefined :: Id y -> Id y))
                          (ntPos iv)
                          (idxStrng1 1 31 iv)
{-# NoInline runInsideBacktrack #-}

-- @TODO throw exception if non of those case?
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

evalIP :: Ptr() -> BS.ByteString -> Int -> Int -> Int -> Int -> Energy
evalIP  c input ((+1 ) -> i) ( (+1) -> j) ((+1) -> k) ((+1) -> l) = V.intLoopCP c i j k l
--evalIP input ((+1 ) -> i) ( (+1) -> j) ((+1) -> k) ((+1) -> l) = if i < j && k < l
--  then V.intLoopP input i j k l
--  else error ("evalIP :: positions messed up (i,j,k,l): " ++ show (i,j,k,l) )

evalHP :: Ptr() -> BS.ByteString -> Int -> Int -> Energy
evalHP c input ((+1) -> i) ((+1) -> j) = V.hairpinCP c i j

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
