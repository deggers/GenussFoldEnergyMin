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

[formalLanguage|
Verbose

Grammar: EnergyMin
N: Struct

T: ntL
T: ntR
T: LntR
T: LntR
T: nt

S: Struct

Struct -> ssl <<< ntL Struct nt
Struct -> ssr <<< Struct nt
Struct -> unp <<< ntR Struct ntL
Struct -> hl <<< ntR Struct ntL
Struct -> sr <<< LntR Struct LntR
Struct -> nil <<< e

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin

openPenalty = 4.09
ignore      = 888888.00

-- Use domain-specific language
type Pos = Int
type Nt = Char
type NtPos = (Nt, Pos)
type MaybeNtPos = (Maybe Nt, Pos)
type Basepair = (Nt, Nt)
type Energy = Double

energyMinAlg :: Monad m => SigEnergyMin m
  Double
  Double
  (MaybeNtPos, NtPos, MaybeNtPos)
  NtPos
  (MaybeNtPos, NtPos)
  (NtPos, MaybeNtPos)
energyMinAlg = SigEnergyMin
  { nil  = \ ()
         -> 0.00

  , ssr  = \ ss _
         -> ss

  , ssl  = \ (_ , (a, aPos)) ss (b, bPos)
         -> if pairs a b
            then ss + openPenalty - (-0.5) -- openePenalty correct here ?
            else ignore

  , hl   = \ _ ss _ -> ss
  , sr   = \ ((maybeA, aPos) , (b,bPos) , (maybeC,cPos))
             ss
             ((maybeD, dPos), (e,ePos) , (maybeF,fPos))
         -> if pairs (b :: Nt) (e ::Nt)
            then ss + energyStem maybeA b maybeC maybeD e maybeF
            else 888888.00

--  , blg  = \ a b ss c -> if pairs (fst a) (fst c) then ss + energyBulge 1 1 else -88888
-- unp :: TERMINAL MISMATCH
  , unp  = \ _ ss _ -> ss

  , h    =   SM.foldl' min (999998.00)
  }
{-# INLINE energyMin #-}

checkHairpin :: MaybeNtPos -> NtPos -> NtPos -> MaybeNtPos -> Bool
checkHairpin _ _ _ _ = False

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
-- xCGx :: Not enough information to calculate energy
-- Otherwise there are 2 basepairs and check if its opening or consecutive
--energyStem (Just a) b c d e (Just f) = if consecutiveBasepairs (b,e) (c,d)
--                                   then stackingEnergies (b,e) (c,d)
--                                   else if pairs b c
--                                   then 4.09  --Opening penalty
--                                   else 88888
energyStem _ _ _ _ _ _ = -2
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

prettyChar :: Monad m => SigEnergyMin m
  [String]
  [[String]]
  (MaybeNtPos, NtPos, MaybeNtPos)
  NtPos
  (MaybeNtPos, NtPos)
  (NtPos, MaybeNtPos)
prettyChar = SigEnergyMin
  { nil = \ () -> [""]
  , ssr = \           [ss] (a, aPos)  -> [       ss ++ [a]]
  , ssl = \ (_ ,(a, aPos)) [ss] _            -> [[a] ++ ss]
  , hl = \    (l, lx) [ss] (xr, r)    -> ["(" ++ ss ++ ")"]
  , sr = \       _ [ss] _           -> ["-(" ++ ss ++ ")-"]
  , unp = \ _ [ss] _  -> [ss]
--  , blg = \       _ _ [ss] _          -> ["_(" ++ ss ++ ")_"]
  , h   = SM.toList
  }
{-# INLINE prettyChar #-}

prettyStructCharShort :: Monad m => SigEnergyMin m
  [String]
  [[String]]
  (MaybeNtPos, NtPos, MaybeNtPos)
  NtPos
  (MaybeNtPos, NtPos)
  (NtPos, MaybeNtPos)
prettyStructCharShort = SigEnergyMin
  { nil = \ () ->  [""]
-- SSR :: Single Stranded Right
  , ssr = \ [ss] (a, aPos) ->  [ss ++ "SSR('" ++ [a] ++ "':" ++ show aPos ++ ") "]
-- SSL :: Single Stranded LEFT
  , ssl = \  (_, (a, aPos)) [ss] (b, bPos) ->  ["STEM('" ++ [a] ++ "':" ++ show aPos ++",'" ++ [b] ++ show bPos ++") " ++ ss]
-- HPL :: Hairpin Loop
  , hl = \((a, aPos), (b, bPos)) [ss] ((c, cPos),(d, dPos))
       ->   ["HPL('" ++ [a] ++ "':" ++ show aPos ++ ",'" ++ [d] ++ "':" ++ show dPos ++ ") " ++ ss ]
-- STEM :: Connected Region
  , sr = \  (_, (b,bPos), _) [ss] (_, (e,ePos), _)
    -> ["STEM(" ++ [b] ++ show bPos ++ ":" ++ [e] ++ show ePos ++ ")" ++ ss]
-- UNP :: Unpaired pairs || TODO :: needed with SSR ?
  , unp = \ ((a,aPos), _) [ss] (_ ,(b, bPos))
        ->  ["UNP('" ++  [a] ++ "':" ++ show aPos ++ ",'" ++ [b] ++ "':" ++ show bPos ++ ") " ++ ss ]

--  , blg = \ (a,aPos) _ [ss] (b, bPos)  -> ["BULGE('" ++ [a] ++ "':" ++ show aPos ++ ",'" ++ [b] ++ "':" ++ show bPos ++ ") " ++ ss]
  , h   = SM.toList
  }
{-# INLINE prettyStructCharShort #-}

pairs :: Char -> Char -> Bool
pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
{-# INLINE pairs #-}

energyMin :: Int -> String -> (Double,[[String]])
energyMin k inp = (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t) = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t)
{-# NOINLINE energyMinAlg #-}

type X = ITbl Id Unboxed Subword Double

-- Because of "nt ss nt" it's safe, that a _left nt_ has a _right nt_ regardless of the inner ss
chrUnsafeRight :: VG.Vector v x => v x -> Chr ((x, Int), (x, Int)) x
chrUnsafeRight xs = Chr f xs where
  f xs k = ( (VG.unsafeIndex xs k, k)
           , (VG.unsafeIndex xs (k+1), k+1)
           )

chrUnsafeLeft :: VG.Vector v x => v x -> Chr ((x, Int ),(x, Int)) x
chrUnsafeLeft xs = Chr f xs where
  f xs k = ( (VG.unsafeIndex xs (k-1), k-1)
           , (VG.unsafeIndex xs k, k)
           )

-- | @Chr@ with its index.
-- This version exposes the index, where the character @x@ is located on the input vector.
chrIx :: VG.Vector v x => v x -> Chr (x, Int)  x
{-# Inline chrIx #-}
chrIx xs = Chr f xs where
  {-# Inline [0] f #-}
  f xs k = (VG.unsafeIndex xs k, k)

chrMaybeRight :: VG.Vector v x => v x -> Chr ((x,Int), (Maybe x,Int)) x
{-# Inline chrMaybeRight #-}
chrMaybeRight xs = Chr f xs where
  {-# Inline [0] f #-}
  f xs k = ( (VG.unsafeIndex xs k, k)
           , (xs VG.!? (k+1), k+1)
           )


chrMaybeLeft :: VG.Vector v x => v x -> Chr ((Maybe x,Int), (x,Int)) x
{-# Inline chrMaybeLeft #-}
chrMaybeLeft xs = Chr f xs where
  {-# Inline [0] f #-}
  f xs k = ( (xs VG.!? (k-1), k-1)
           , (VG.unsafeIndex xs k, k)
           )

maybeLeftChrMaybeRight :: VG.Vector v x => v x -> Chr ((Maybe x,Int), (x,Int), (Maybe x, Int)) x
{-# Inline maybeLeftChrMaybeRight #-}
maybeLeftChrMaybeRight xs = Chr f xs where
  {-# Inline [0] f #-}
  f xs k = ( (xs VG.!? (k-1), k-1)
           , (VG.unsafeIndex xs k, k)
           , (xs VG.!? (k+1), k+1)
           )

runInsideForward :: VU.Vector Char -> Z:.X
runInsideForward i = mutateTablesWithHints (Proxy :: Proxy CFG)
                   $ gEnergyMin energyMinAlg
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999.0) []))
                        (maybeLeftChrMaybeRight i)
                        (chrIx i)
                        (chrMaybeLeft i)
                        (chrMaybeRight i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X -> [[String]]
runInsideBacktrack i (Z:.t) = unId $ axiom b
  where !(Z:.b) = gEnergyMin (energyMinAlg <|| prettyStructCharShort)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (maybeLeftChrMaybeRight i)
                          (chrIx i)
                          (chrMaybeLeft i)
                          (chrMaybeRight i)
{-# NoInline runInsideBacktrack #-}
