module BioInf.GenussFold.SimpleEnergyStackedHairpinLoops where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
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

-- | Define signature and grammar
{-  Next Nonterminals
-}
[formalLanguage|
Verbose

Grammar: EnergyMin
N: a_Struct
N: b_Closed
N: c_Region
N: d_Region3


T: nt

S: a_Struct

a_Struct -> unpaired <<< nt a_Struct
a_Struct -> paired   <<< nt b_Closed nt a_Struct
a_Struct -> nil      <<< e

b_Closed -> hairpin  <<< nt d_Region3 nt
b_Closed -> interior <<< c_Region nt b_Closed nt c_Region
b_Closed -> nil      <<< e

c_Region -> region   <<< nt c_Region
c_Region -> nil      <<< e

d_Region3 -> region3 <<< nt nt nt c_Region

//
Emit: EnergyMin
|]

{-
N: e_M
N: f_M1


e_M -> mcm_1 <<< c_Region b_Closed
e_M -> mcm_2 <<< e_M b_Closed
e_M -> mcm_3 <<< e_M nt
e_M -> nil <<< e

f_M1 -> ocm_1 <<< f_M1 nt
f_M1 -> ocm_2 <<< b_Closed
f_M1 -> nil <<< e
-}

makeAlgebraProduct ''SigEnergyMin

energyMinAlg :: Monad m => SigEnergyMin m Double Double (MaybeNtPos, NtPos, MaybeNtPos)
energyMinAlg = SigEnergyMin
  { nil  = \ () -> 0.00
  , unpaired = \ _ ss -> ss
  , paired = \ (_,(a,aPos),_) x (_, (b,bPos), _) y -> if pairs a b
                                                      then x + y - 1
                                                      else ignore
  , hairpin = \ (_,(a,aPos),_) ss (_,(b,bPos),_)  -> if pairs a b then ss - 1 else ignore
  , interior = \ _ (_,(a,aPos),_) ss (_,(b,bPos),_) _ -> if pairs a b
                                                         then ss - 1
                                                         else ignore
  {-
  , unk_ml = \ m m1 -> ignore
-- Multi Component Multiloop2Case 1 :: mcm_1 <<< Region Closed
  , mcm_1 = \ region closed-> ignore
-- Multi Component Multiloop Case 2 :: mcm_2 <<< M Closed
  , mcm_2 = \ m closed -> ignore -- Multi Component Multiloop Case 3 :: mcm_3 <<< M nt
  , mcm_3 = \ m _ -> ignore
-- One Component Multiloop Case 1 :: ocm_1 <<< M1 nt
  , ocm_1 = \  m1 _ -> ignore
-- One Component Multiloop Case 2 :: ocm_2 <<< Closed
  , ocm_2 = \ closed -> ignore
-}
  , region  = \ _ ss      -> ss
  , region3 = \ _ _ _ ss -> ss
  , h    =   SM.foldl' min (ignore)
  }
{-# INLINE energyMin #-}

prettyStructCharShort :: Monad m => SigEnergyMin m [String] [[String]] (MaybeNtPos, NtPos, MaybeNtPos)
prettyStructCharShort = SigEnergyMin
  { nil = \ () ->  [""]
  , unpaired = \ (_, (a, aPos), _) [ss] -> ["." ++ ss]
  , paired = \ (_,(a,aPos),_) [x] (_,(b,bPos),_) [y] -> ["(" ++ x ++ ")" ++ y]
  , hairpin = \  _ [ss] _  -> ["(" ++ss ++")"]
  , interior = \ [x] a [ss] b [y] -> [x ++ "(" ++ ss ++ ")" ++ y]
{-
  , unk_ml = \ [m] [m1] -> ["unk.."]
  , mcm_1 = \ [region] [closed] -> [".."]
  , mcm_2 = \ [m] [closed] -> [".."]
  , mcm_3 = \ [m] _ -> [".."]
  , ocm_1 = \ [m1] _ -> [".."]
  , ocm_2 = \ [closed] -> ["..."]
-}
  , region = \ _ [ss] -> ["." ++ ss]
  , region3 = \ _ _ _ [ss] -> ["..." ++ ss]
  , h   = SM.toList
  }
{-# INLINE prettyStructCharShort #-}

openPenalty = 4.09
ignore      = 9999999.00

-- Use domain-specific language
type Pos = Int
type Nt = Char
type NtPos = (Nt, Pos)
type MaybeNtPos = (Maybe Nt, Pos)
type Basepair = (Nt, Nt)
type Energy = Double

energyMin :: Int -> String -> (Double,[[String]])
energyMin k inp = (z, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  !(Z:.a:.b:.c:.d) = runInsideForward i
  z = unId $ axiom a
  bs = runInsideBacktrack i (Z:.a:.b:.c:.d)
{-# NOINLINE energyMinAlg #-}

type X = ITbl Id Unboxed Subword Double
-- PK will ned subwords over subwords

--runInsideForward :: VU.Vector Char -> Z:.X
runInsideForward i = mutateTablesWithHints (Proxy :: Proxy CFG)
                   $ gEnergyMin energyMinAlg
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-166999.0) []))
                        (ITbl 0 1 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-266999.0) []))
                        (ITbl 0 2 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-366999.0) []))
                        (ITbl 0 3 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-466999.0) []))
                        (maybeLeftChrMaybeRight i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X:.X:.X:.X -> [[String]]
runInsideBacktrack i (Z:.a:.b:.c:.d) = unId $ axiom g -- Axiom from the Start Nonterminal S -> a_Struct
  where !(Z:.g:.h:.j:.k) = gEnergyMin (energyMinAlg <|| prettyStructCharShort)
                          (toBacktrack a (undefined :: Id a -> Id a))
                          (toBacktrack b (undefined :: Id b -> Id b))
                          (toBacktrack c (undefined :: Id c -> Id c))
                          (toBacktrack d (undefined :: Id d -> Id d))
                          (maybeLeftChrMaybeRight i)
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

energyHairpinLoop :: NtPos -> NtPos -> NtPos -> NtPos -> Energy
energyHairpinLoop (a,aPos) (b, bPos) (c,cPos) (d,dPos) = case dPos - aPos of
  4 -> 1
  5 -> 4
  6 -> 7
  _ -> 999999

-- if left first and left right can pair is not initiation otherwise it iis and needs penalty :: checkFirstPairing
-- if checkFirstPairing then addPairingPenalty else 0
-- get energy which depends on loop of 4 nt -> (this,this) (next,next)
energyStem :: Maybe Nt -> Nt -> Maybe Nt ->Maybe Nt -> Nt -> Maybe Nt -> Energy
-- Otherwise there are 2 basepairs and check if its opening or consecutive
energyStem (Just a) b (Just c)
           (Just d) e (Just f) = if consecutiveBasepairs (a,f) (b,e)
                                   then stackingEnergies (a,f) (b,e)
                                   else if pairs b e
                                   then 4.09  --Opening penalty
                                   else ignore
energyStem Nothing  b (Just c)
           (Just d) e Nothing = if pairs b e
                                then 4.09 --opening penalty
                                else ignore
energyStem _ _ _ _ _ _ = ignore
--energyStem (Just a) b c d e (Just f) = if pairs a f
--                                       then stackingEnergies (b,e) (c,d)
 --                                      else stackingEnergies (b,e) (c,d) + openPenalty

--energyStem Nothing a b c d Nothing = stackingEnergies (a,d) (b,c) + openPenalty
--energyStem Nothing a b c d (Just e) = 88 -- plus maybe dangling end energy
--energyStem (Just a) b c d e Nothing = 88

consecutiveBasepairs :: Basepair -> Basepair -> Bool
consecutiveBasepairs (a,d) (b,c) = if pairs a d && pairs b c
                                   then True
                                   else False


stackingEnergies :: Basepair -> Basepair -> Double
-- AU XY
stackingEnergies ('A','U') ('C','U') = -0.9
stackingEnergies ('A','U') ('G','U') = -0.6
stackingEnergies ('A','U') ('C','G') = -2.2
stackingEnergies ('A','U') ('U','G') = -1.4
stackingEnergies ('A','U') ('G','C') = -2.1
stackingEnergies ('A','U') ('U','A') = -1.1
stackingEnergies ('A','U') ('A','U') = -0.93
-- CU XY
stackingEnergies   _   _  = -0.9

energyBulge :: Int -> Int -> Energy
energyBulge a b = case b-a of
  1 -> 1
  2 -> 4
  3 -> 11
  _ -> -88888

maybeLeftChrMaybeRight :: VG.Vector v x => v x -> Chr ((Maybe x,Int), (x,Int), (Maybe x, Int)) x
{-# Inline maybeLeftChrMaybeRight #-}
maybeLeftChrMaybeRight xs = Chr f xs where
  {-# Inline [0] f #-}
  f xs k = ( (xs VG.!? (k-1), k-1)
           , (VG.unsafeIndex xs k, k)
           , (xs VG.!? (k+1), k+1)
           )

{-
choener
Weak -> ( unpaired_region )
Strong -> ( Weak )
Strong -> ( Strong )
Exterior -> Strong
Exterior -> nt Strong nt
Exterior -> Region String Region
(((...)))[[[...]]]
Open -> nt region nt
xxx
Hairpin:
Open -> nt region nt
Open -> nt region2 Closed region2 nt
(.......)
[...((???)).....]
...
Closed -> nt Closed nt
Closed -> nt Open nt
https://link.springer.com/article/10.1007/s00285-007-0107-5
https://github.com/choener/ADPfusion/blob/master/ADPfusion/Core/Term/Str.hs
http://hackage.haskell.org/package/ADPfusion-0.5.1.0/docs/src/ADP-Fusion-Term-Strng-Subword.html
Region -> nt
Region -> nt Region
<<<>>>
-}

{-
Insights from the Bompfwürnerwer Paper

- From the biophysical point of view one distinguishes hairpin loops, stackedbase pairs, bulges, true interior loops, and multi(branched) loops.
- From an algorithmicpoint of view one can treat bulges, stacked pairs, and true interior loops as _subtypes_ ofinterior loops.



-}
