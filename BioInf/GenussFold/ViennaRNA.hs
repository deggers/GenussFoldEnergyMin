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
N: c_M
N: d_M1

T: nt
T: regionCtx

S: a_Struct

a_Struct -> unpaired     <<< nt a_Struct
a_Struct -> juxtaposed   <<< b_Closed a_Struct
a_Struct -> nil          <<< e

b_Closed -> hairpin      <<< regionCtx
b_Closed -> interior     <<< regionCtx b_Closed regionCtx
b_Closed -> mlr          <<< nt c_M d_M1 nt

c_M -> mcm_1             <<< regionCtx b_Closed
c_M -> mcm_2             <<< c_M nt b_Closed nt
c_M -> mcm_3             <<< c_M nt

d_M1 -> ocm_1            <<< d_M1 nt
d_M1 -> ocm_2            <<< nt b_Closed nt

//
Emit: EnergyMin
|]

-- @TODO  hairpin -> regionCtx 5 ... 32  3 empty 2 paired to ensure only semtical correct parses

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
  , unpaired = \ _ ss -> ss + 1.00

  , juxtaposed   = \ x y -> x + y
  , hairpin  = \ (left, subtract 1 -> right) -> if
             |  (right-left) > 3 && pairs (input VU.! left) (input VU.! right) -> fromIntegral (5 + right-left)
             | otherwise -> ignore

    -- @TODO remove constraints of allowing only stacks no bulges
  , interior = \ (aPos,bPos) closed (subtract 1 -> cPos, subtract 1 -> dPos) -> if
             | (bPos-aPos == 1) && (dPos-cPos == 1) && pairs (input VU.! aPos) (input VU.! dPos) && pairs (input VU.! bPos) (input VU.! cPos) ->
                 closed + interiorLoopEnergy (input VU.! aPos ,input VU.! dPos) (input VU.! bPos, input VU.! cPos)
             | otherwise -> ignore

  , mlr      = \ (a,aPos) m m1 (d,dPos) -> if
             | pairs a d -> m + m1 + 3
             | otherwise   -> ignore

  , mcm_1 = \ _ closed -> closed

  , mcm_2 = \  m (a,aPos) closed (b,bPos) -> if
          | pairs a b -> m + closed
          | otherwise ->  ignore

  , mcm_3 = \  m _ ->  m
  , ocm_1 = \  m1 _ -> m1
  , ocm_2 = \ (a,aPos) closed (b,bPos) -> if pairs a b then closed else ignore
  , h    =   SM.foldl' min (ignore)
  }
{-# INLINE energyMin #-}

prettyStructCharShort :: Monad m => SigEnergyMin m [String] [[String]] NtPos (Pos,Pos)
prettyStructCharShort = SigEnergyMin
  { nil = \ () ->  [""]
  , unpaired = \ _ [ss] -> ["u" ++ ss]
  , juxtaposed = \ [x] [y] -> [x ++ y]
  , hairpin = \  _  -> ["(. . .)"]
  , interior = \ regionL [closed] regionR -> ["(" ++ show regionL ++ "(" ++ closed ++ ")" ++ show regionR ++ ")" ]
  , mlr = \ _ [m] [m1] _ -> ["(" ++ m ++ m1 ++ ")"]
  , mcm_1 = \ region [closed] -> [ show region ++ closed ]
  , mcm_2 = \ [m] _ [closed] _ -> [m ++ "(" ++ closed ++ ")"]
  , mcm_3 = \ [m] _ -> [m ++ ".m3"]
  , ocm_1 = \ [m1] _ -> [m1 ++ ".o1"]
  , ocm_2 = \ _ [x] _ -> ["(" ++ x ++ ")"]
  , h   = SM.toList
  }
{-# INLINE prettyStructCharShort #-}

prettyPaths :: Monad m => SigEnergyMin m [String] [[String]] NtPos (Pos,Pos)
prettyPaths = SigEnergyMin
  { nil = \ () ->  [""]
  , unpaired = \  _ [ss] ->
      [ss]
  , juxtaposed = \ [x] [y] ->
      [x ++ y]
  , hairpin = \  (left, subtract 1 -> right)  ->
      ["Hairpin Loop (" ++ show left ++ "..." ++ show right ++ ") "]
  , interior = \ regionL [closed] (subtract 1 -> c, subtract 1 -> d) ->
      ["Interior loop (" ++ show regionL ++ ") (" ++ show c ++ "," ++ show d ++  "), " ++ closed]
  , mlr = \ _ [m] [m1] _ ->
      ["mlr " ++ m ++ m1 ++ "mlr "]
  , mcm_1 = \ region [closed] ->
      [ show region ++ closed ]
  , mcm_2 = \ [m] _ [closed] _ -> [m ++ "mcm2 " ++ closed ++  "mcm2"]
  , mcm_3 = \ [m] _ -> [m ++ " mcm3"]
  , ocm_1 = \ [m1] _ -> [m1 ++ "."]
  , ocm_2 = \ _ [x] _ -> ["ocm2 " ++ x ++  "ocm2"]
  , h   = SM.toList
  }
{-# INLINE prettyPaths #-}



energyMin :: Int -> String -> (Double,[[String]])
energyMin k inp = (z, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  !(Z:.a:.b:.e:.f) = runInsideForward i
  z = unId $ axiom a -- gets the value from the table
  bs = runInsideBacktrack i (Z:.a:.b:.e:.f)
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
                        (ntPos i)
                        (idxStrng1 1 31 i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X:.X:.X:.X -> [[String]] -- for the non-terminals
runInsideBacktrack i (Z:.a:.b:.e:.f) = unId $ axiom g -- Axiom from the Start Nonterminal S -> a_Struct-
--  where !(Z:.g:.h:.j:.k:.l:.m) = gEnergyMin (energyMinAlg i <|| prettyStructCharShort)
  where !(Z:.g:.h:.l:.m) = gEnergyMin (energyMinAlg i <|| prettyPaths)
                          (toBacktrack a (undefined :: Id a -> Id a))
                          (toBacktrack b (undefined :: Id b -> Id b))
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
