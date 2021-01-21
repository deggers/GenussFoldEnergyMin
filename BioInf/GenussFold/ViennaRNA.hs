{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module BioInf.GenussFold.ViennaRNA where

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

-- | Define signature and grammar
[formalLanguage|
Verbose

Grammar: EnergyMin
N: a_Struct
N: b_Closed
N: c_Region
N: d_Region3
N: e_M
N: f_M1

T: nt
T: regionCtx

S: a_Struct

a_Struct -> unpaired     <<< nt a_Struct
a_Struct -> juxtaposed   <<< b_Closed a_Struct
a_Struct -> nil          <<< e

b_Closed -> hairpin      <<< nt d_Region3 nt
b_Closed -> interior     <<< regionCtx b_Closed regionCtx
b_Closed -> mlr          <<< nt e_M f_M1 nt

e_M -> mcm_1             <<< c_Region b_Closed
e_M -> mcm_2             <<< e_M nt b_Closed nt
e_M -> mcm_3             <<< e_M nt

f_M1 -> ocm_1            <<< f_M1 nt
f_M1 -> ocm_2            <<< nt b_Closed nt

c_Region -> region       <<< nt c_Region
c_Region -> nil          <<< e

d_Region3 -> region3     <<< nt nt nt c_Region

//
Emit: EnergyMin
|]

--  b_Closed -> interior <<< nt c_Region nt b_Closed nt c_Region nt
  -- Idea is we can have nt . . . maybeNt someClosedStruct maybeNt ... nt

-- hairpin -> regionCtx 5 ... 32  3 empty 2 paired

-- Use domain-specific language
type Pos = Int
type Nt = Char
type NtPos = (Nt, Pos)
type MaybeNtPos = (Maybe Nt, Pos)
type Basepair = (Nt, Nt)
type Energy = Double

type IndexRegionParser = (Pos,Pos)
ignore      = 100123.00

makeAlgebraProduct ''SigEnergyMin

interiorLoopEnergy ::  Basepair -> Basepair -> Double
interiorLoopEnergy ('C','G') ('G','U') = -1.40
interiorLoopEnergy ('G','U') ('C','G') = -2.50
interiorLoopEnergy ('C','G') ('U','A') = -2.10
interiorLoopEnergy ('U','A') ('U','A') = -0.90
interiorLoopEnergy ('C','G') ('A','U') = -2.10
interiorLoopEnergy ('A','U') ('U','A') = -1.10
interiorLoopEnergy ('U','A') ('A','U') = -1.30
interiorLoopEnergy ('U','A') ('C','G') = -2.40
interiorLoopEnergy ('C','G') ('C','G') = -3.30
interiorLoopEnergy ('C','G') ('U','G') = -2.10
interiorLoopEnergy ('U','G') ('A','U') = -1.00
interiorLoopEnergy ('A','U') ('A','U') = -0.90
interiorLoopEnergy ('G','C') ('A','U') = -2.40
interiorLoopEnergy ('A','U') ('G','C') = -2.10
interiorLoopEnergy _ _ = -1 

energyMinAlg :: Monad m => VU.Vector Char ->  SigEnergyMin m Double Double NtPos (Pos,Pos)
energyMinAlg input = SigEnergyMin
  { nil  = \ () -> 0.00
  , unpaired = \ _ ss -> ss + 10.00 -- should be higher than any hairpin penalty, i think

  , juxtaposed   = \ x y -> x + y
  , hairpin  = \ (a,aPos) ss (b,bPos)  -> if
             | pairs a b -> ss + 5.0
             | otherwise -> ignore

-- b_Closed -> interior <<< regionCtx b_Closed regionCtx
  , interior = \ (aPos,bPos) closed (subtract 1 -> cPos, subtract 1 -> dPos) -> if   -- i VU.! a/b/c/d
             | pairs (input VU.! aPos) (input VU.! dPos) && pairs (input VU.! bPos) (input VU.! cPos) -> closed - 1 -- + interiorLoopEnergy (a,d) (b,c) -- + fromIntegral (bPos-aPos-1) + fromIntegral (dPos-cPos-1)  -- calculate energy with InteriorLoop a d b c 0
             | otherwise -> ignore

  , mlr      = \ (a,aPos) m m1 (d,dPos) -> if
             | pairs a d -> m + m1 - 1  -- energy of an interiorLoop a d b c   but only iff it pairs? otherwise search until found? with regionCtx?
             | otherwise   -> ignore

  , mcm_1 = \ region closed -> region + closed

  , mcm_2 = \  m (a,cPos) closed (b,dPos) -> if
          | pairs a b -> m + closed + 4.5
          | otherwise ->  ignore

  , mcm_3 = \  m _ ->  m
  , ocm_1 = \  m1 _ -> m1
  , ocm_2 = \ (a,aPos) closed (b,bPos) -> if pairs a b then closed else ignore
  , region  = \ _ ss      -> ss
  , region3 = \ _ _ _ ss -> ss
  , h    =   SM.foldl' min (ignore)
  }
{-# INLINE energyMin #-}

prettyStructCharShort :: Monad m => SigEnergyMin m [String] [[String]] NtPos (Pos,Pos)
prettyStructCharShort = SigEnergyMin
  { nil = \ () ->  [""]
  , unpaired = \ (a, aPos) [ss] -> ["u" ++ ss]
  , juxtaposed = \ [x] [y] -> [x ++ y]
  , hairpin = \  _ [region] _  -> ["(" ++ region ++")"]
  , interior = \ regionL [closed] regionR -> ["(" ++ show regionL ++ "(" ++ closed ++ ")" ++ show regionR ++ ")" ]
  , mlr = \ _ [m] [m1] _ -> ["(" ++ m ++ m1 ++ ")"]
  , mcm_1 = \ [region] [closed] -> [ region ++ closed ]
  , mcm_2 = \ [m] _ [closed] _ -> [m ++ "(" ++ closed ++ ")"]
  , mcm_3 = \ [m] _ -> [m ++ ".m3"]
  , ocm_1 = \ [m1] _ -> [m1 ++ ".o1"]
  , ocm_2 = \ _ [x] _ -> ["(" ++ x ++ ")"]
  , region = \ _ [ss] -> ["r" ++ ss]
  , region3 = \ _ _ _ [ss] -> ["rrr" ++ ss]
  , h   = SM.toList
  }
{-# INLINE prettyStructCharShort #-}

prettyPaths :: Monad m => SigEnergyMin m [String] [[String]] NtPos (Pos,Pos)
prettyPaths = SigEnergyMin
  { nil = \ () ->  [" nil"]
  , unpaired = \  _ [ss] ->
      ["u" ++ ss]
  , juxtaposed = \ [x] [y] ->
      [x ++ y]
  , hairpin = \  left [region] right  ->
      ["Hairpin Loop (" ++ show left ++ "," ++ show right ++ ") " ++ region]
  , interior = \ regionL [closed] (subtract 1 -> c, subtract 1 -> d) ->
      ["Interior loop (" ++ show regionL ++ ") (" ++ show c ++ "," ++ show d ++  "), " ++ closed]
  , mlr = \ _ [m] [m1] _ ->
      ["mlr " ++ m ++ m1 ++ "mlr "]
  , mcm_1 = \ [region] [closed] ->
      [ region ++ closed ]
  , mcm_2 = \ [m] _ [closed] _ -> [m ++ "mcm2 " ++ closed ++  "mcm2"]
  , mcm_3 = \ [m] _ -> [m ++ " mcm3"]
  , ocm_1 = \ [m1] _ -> [m1 ++ "."]
  , ocm_2 = \ _ [x] _ -> ["ocm2 " ++ x ++  "ocm2"]
  , region = \ _ [ss] -> ["r" ++ ss]
  , region3 = \ _ _ _ [ss] -> ["..." ++ ss]
  , h   = SM.toList
  }
{-# INLINE prettyPaths #-}



energyMin :: Int -> String -> (Double,[[String]])
energyMin k inp = (z, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  !(Z:.a:.b:.c:.d:.e:.f) = runInsideForward i
  z = unId $ axiom a -- gets the value from the table
  bs = runInsideBacktrack i (Z:.a:.b:.c:.d:.e:.f)
{-# NOINLINE energyMinAlg #-}

type X = ITbl Id Unboxed Subword Double
-- PK will need subwords over subwords

--runInsideForward :: VU.Vector Char -> Z:.X
runInsideForward i = mutateTablesWithHints (Proxy :: Proxy CFG)
                   $ gEnergyMin (energyMinAlg i)
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (166999.0) []))
                        (ITbl 0 1 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (266999.0) []))
                        (ITbl 0 2 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (366999.0) []))
                        (ITbl 0 3 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (466999.0) []))
                        (ITbl 0 4 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (566999.0) []))
                        (ITbl 0 5 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (666999.0) []))
                        (ntPos i)
                        (idxStrng1 1 31 i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X:.X:.X:.X:.X:.X -> [[String]] -- for the non-terminals
runInsideBacktrack i (Z:.a:.b:.c:.d:.e:.f) = unId $ axiom g -- Axiom from the Start Nonterminal S -> a_Struct-
--  where !(Z:.g:.h:.j:.k:.l:.m) = gEnergyMin (energyMinAlg i <|| prettyStructCharShort)
  where !(Z:.g:.h:.j:.k:.l:.m) = gEnergyMin (energyMinAlg i <|| prettyPaths)
                          (toBacktrack a (undefined :: Id a -> Id a))
                          (toBacktrack b (undefined :: Id b -> Id b))
                          (toBacktrack c (undefined :: Id c -> Id c))
                          (toBacktrack d (undefined :: Id d -> Id d))
                          (toBacktrack e (undefined :: Id e -> Id e))
                          (toBacktrack f (undefined :: Id f -> Id f))
                          (ntPos i)
                          (idxStrng 1 31 i)
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
