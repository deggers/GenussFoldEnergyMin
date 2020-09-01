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
T: ntMaybeL
T: ntMaybeR
T: nt

S: Struct

Struct -> ssl <<< nt Struct
Struct -> ssr <<< Struct nt
Struct -> unp <<< nt Struct nt
Struct -> hl <<< ntR Struct ntL
Struct -> sr <<< ntMaybeL Struct ntMaybeR
Struct -> nil <<< e

//
Emit: EnergyMin
|]

-- Working Example : UUCGGAGGAUGGCUCAUCAAGCUAAGUAAGGAGCUCCCUGAAGCCAUAUCCGAGGC
--                   ((((((..((((((((...((((........))))...)).)))))).))))))..
-- 	Bovine foamy virus miR-BF1 stem-loop :: MI0030366
{-
  External loop                           :   -30
  Interior loop (  1, 54) UG; (  2, 53) UA:   -60
  Interior loop (  2, 53) UA; (  3, 52) CG:  -240
  Interior loop (  3, 52) CG; (  4, 51) GC:  -240
  Interior loop (  4, 51) GC; (  5, 50) GC:  -330
  Interior loop (  5, 50) GC; (  6, 49) AU:  -240
  Interior loop (  6, 49) AU; (  9, 47) AU:   260
  Interior loop (  9, 47) AU; ( 10, 46) UA:  -110
  Interior loop ( 10, 46) UA; ( 11, 45) GC:  -210
  Interior loop ( 11, 45) GC; ( 12, 44) GC:  -330
  Interior loop ( 12, 44) GC; ( 13, 43) CG:  -340
  Interior loop ( 13, 43) CG; ( 14, 42) UA:  -210
  Interior loop ( 14, 42) UA; ( 15, 40) CG:   140
  Interior loop ( 15, 40) CG; ( 16, 39) AU:  -210
  Interior loop ( 16, 39) AU; ( 20, 35) AU:   340
  Interior loop ( 20, 35) AU; ( 21, 34) GC:  -210
  Interior loop ( 21, 34) GC; ( 22, 33) CG:  -340
  Interior loop ( 22, 33) CG; ( 23, 32) UA:  -210
  Hairpin  loop ( 23, 32) UA              :   500
-}

makeAlgebraProduct ''SigEnergyMin

-- Use domain-specific language
type Pos = Int
type Nt = Char
type NtPos = (Nt, Pos)
type Basepair = (Nt, Nt)
type Energy = Double

energyMinAlg :: Monad m => SigEnergyMin m Double Double NtPos (NtPos, NtPos) ((Maybe Char, Int), NtPos) (NtPos, (Maybe Char, Int)) (NtPos, NtPos)
energyMinAlg = SigEnergyMin
  { nil  = \           ()                      -> 0.00
  , ssr  = \           ss (b,bPos)             -> ss
  , ssl  = \  (a,aPos) ss                      -> ss
  , hl   = \     (a,b) ss (c, d)               -> if checkHairpin a b c d
                                                  then energyHairpinLoop a b c d + ss
                                                  else 888888.00
  , sr   = \ ((maybeA, maybeAPos) ,(b, bPos))
             ss
             ((c, cPos), (maybeD,maybeDPos))  -> if pairs b c
                                                 then ss + energyStem maybeA b c maybeD
                                                 else 888888.00
--  , blg  = \ a b ss c -> if pairs (fst a) (fst c) then ss + energyBulge 1 1 else -88888
  , unp  = \ (a, aPos) ss (b, bPos)           -> if not (pairs a b) then ss else ss
  , h    =   SM.foldl' min (999998.00)
  }
{-# INLINE energyMin #-}

checkHairpin :: NtPos -> NtPos -> NtPos -> NtPos -> Bool
checkHairpin (a,aPos) (b, bPos) (c,cPos) (d,dPos) = if pairs a d && not (pairs b c) then True else False

energyHairpinLoop :: NtPos -> NtPos -> NtPos -> NtPos -> Energy
energyHairpinLoop (a,aPos) (b, bPos) (c,cPos) (d,dPos) = case dPos - aPos of
  4 -> 1
  5 -> 4
  6 -> 7
  _ -> 999999


-- if left first and left right can pair is not initiation otherwise it iis and needs penalty :: checkFirstPairing
-- if checkFirstPairing then addPairingPenalty else 0
-- get energy which depends on loop of 4 nt -> (this,this) (next,next)
energyStem :: Maybe Nt -> Nt -> Nt -> Maybe Nt -> Energy
energyStem _ a b _ = -2


energyBulge :: Int -> Int -> Energy
energyBulge a b = case b-a of
  1 -> 1
  2 -> 4
  3 -> 11
  _ -> -88888

prettyChar :: Monad m => SigEnergyMin m [String] [[String]] NtPos (NtPos, NtPos) (Maybe NtPos, NtPos) (NtPos, Maybe NtPos) (NtPos, NtPos)
prettyChar = SigEnergyMin
  { nil = \ () -> [""]
  , ssr = \           [ss] (a, aPos)  -> [       ss ++ [a]]
  , ssl = \ (a, aPos) [ss]            -> [[a] ++ ss]
  , hl = \    (l, lx) [ss] (xr, r)    -> ["(" ++ ss ++ ")"]
  , sr = \        _ [ss] _            -> ["-(" ++ ss ++ ")-"]
  , unp = \ (a, aPos) [ss] (b, bPos)  -> [[a] ++ ss ++ [b]]
--  , blg = \       _ _ [ss] _          -> ["_(" ++ ss ++ ")_"]
  , h   = SM.toList
  }
{-# INLINE prettyChar #-}

prettyStructCharShort :: Monad m => SigEnergyMin m [String] [[String]] NtPos (NtPos, NtPos) ((Maybe Char, Int), NtPos) (NtPos, (Maybe Char, Int)) (NtPos, NtPos)
prettyStructCharShort = SigEnergyMin
  { nil = \ () -> [""]
  , ssr = \           [ss] (a, aPos)  -> [       ss ++ "SSR('" ++ [a] ++ "') "]
  , ssl = \ (a, aPos) [ss]            -> ["SSL('" ++ [a] ++ "':" ++ show aPos ++ ") " ++ ss]
  , hl = \  ((a, aPos), (b, bPos)) [ss] ((c, cPos),(d, dPos))  -> ["HPL('" ++ [a] ++ "':" ++ show aPos ++ ",'" ++ [d] ++ "':" ++ show dPos ++ ") " ++ ss ]
  , sr = \  (_, (a,aPos)) [ss] ((b,bPos), _)  -> ["STEM('" ++ [a] ++ "':" ++ show aPos ++ ",'" ++ [b] ++ "':" ++ show bPos ++ ") " ++ ss]
  , unp = \ (a, aPos) [ss] (b, bPos)  -> ["UNP('" ++  [a] ++ "':" ++ show aPos ++ ",'" ++ [b] ++ "':" ++ show bPos ++ ") " ++ ss ]
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

stackingEnergies :: Basepair -> Basepair -> Double
-- AU XY
stackingEnergies ('A','U') ('C','U') = -0.9
stackingEnergies ('A','U') ('G','U') = -0.6
stackingEnergies ('A','U') ('C','G') = -2.2
stackingEnergies ('A','U') ('U','G') = -1.4
stackingEnergies ('A','U') ('G','C') = -2.1
stackingEnergies ('A','U') ('U','A') = -1.1
-- CU XY
stackingEnergies   _   _  = 0

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

runInsideForward :: VU.Vector Char -> Z:.X
runInsideForward i = mutateTablesWithHints (Proxy :: Proxy CFG)
                   $ gEnergyMin energyMinAlg
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999.0) []))
                        (chrIx i)
                        (chrUnsafeLeft i)
                        (chrMaybeLeft i)
                        (chrMaybeRight i)
                        (chrUnsafeRight i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X -> [[String]]
runInsideBacktrack i (Z:.t) = unId $ axiom b
  where !(Z:.b) = gEnergyMin (energyMinAlg <|| prettyStructCharShort)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (chrIx i)
                          (chrUnsafeLeft i)
                          (chrMaybeLeft i)
                          (chrMaybeRight i)
                          (chrUnsafeRight i)
{-# NoInline runInsideBacktrack #-}
