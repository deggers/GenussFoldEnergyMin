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
T: nt
S: Struct

Struct -> unp <<< nt Struct nt
Struct -> ssr <<< Struct nt
Struct -> ssl <<< nt Struct
Struct -> hl <<< ntR Struct ntL
Struct -> stem <<< nt Struct nt
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

type NtPos = (Char,Int)

energyMinAlg :: Monad m => SigEnergyMin m Int Int Char (NtPos, NtPos) (NtPos, NtPos)
energyMinAlg = SigEnergyMin
  { ssr = \ x c                -> x
  , ssl = \ c x                -> x
  , nil = \ ()                 -> 0
  , hl =  \ (a,b) ss (c, d) -> if checkHairpin a b c d then ss + energyHairpinLoop a b c d else -888888
  , stem = \ l ss r            -> if pairs l r then ss + 1 else -888888
  , unp = \ a ss b             -> if not (pairs a b) then ss else ss
  , h   = SM.foldl' max (-999998)
  }
{-# INLINE energyMin #-}

checkHairpin :: NtPos -> NtPos -> NtPos -> NtPos -> Bool
checkHairpin (a,aPos) (b, bPos) (c,cPos) (d,dPos) = if pairs a d && dPos - aPos > 3 && not (pairs b c) then True else False

energyHairpinLoop :: NtPos -> NtPos -> NtPos -> NtPos -> Int
energyHairpinLoop (l,lPos) (mR, mRPos) mL r = 1

prettyChar :: Monad m => SigEnergyMin m [String] [[String]] Char (NtPos, NtPos) (NtPos, NtPos)
prettyChar = SigEnergyMin
  { ssr = \ [xs] r              -> [xs ++ [r]]
  , ssl = \ l [xs]              -> [[l] ++ xs]
  , nil = \ ()                  -> [""]
  , hl = \ (l, lx) [xs] (xr, r) -> ["(" ++ xs ++ ")"]
  , stem = \ _ [xs] _           -> ["(" ++ xs ++ ")"]
  , unp = \ l [xs] r            -> [[l] ++ xs ++ [r]]
  , h   = SM.toList
  }
{-# INLINE prettyChar #-}

-- @TODO implement missing entropie parameters
-- @TODO ent_hl :: LoopSize -> Energy
ent_hl :: Int -> Double
ent_hl 3 = 4.1
ent_hl 4 = 4.9
ent_hl _ = error "not implemented yet"

pairs :: Char -> Char -> Bool
pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
{-# INLINE pairs #-}

energyMin :: Int -> String -> (Int,[[String]])
energyMin k inp = (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t) = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t)
{-# NOINLINE energyMinAlg #-}

type X = ITbl Id Unboxed Subword Int

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

runInsideForward :: VU.Vector Char -> Z:.X
runInsideForward i = mutateTablesWithHints (Proxy :: Proxy CFG)
                   $ gEnergyMin energyMinAlg
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (chr i)
                        (chrUnsafeLeft i)
                        (chrUnsafeRight i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X -> [[String]]
runInsideBacktrack i (Z:.t) = unId $ axiom b
  where !(Z:.b) = gEnergyMin (energyMinAlg <|| prettyChar)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (chr i)
                          (chrUnsafeLeft i)
                          (chrUnsafeRight i)
{-# NoInline runInsideBacktrack #-}
