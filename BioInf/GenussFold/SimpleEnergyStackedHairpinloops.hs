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
Struct -> nil <<< e

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin

energyMinAlg :: Monad m => SigEnergyMin m Int Int Char (Maybe Char, Char) (Char, Maybe Char)
energyMinAlg = SigEnergyMin
  { ssr = \ x c      -> x
  , ssl = \ c x      -> x
  , nil = \ ()       -> 0
  , hl =  \ (a,ma) ss (mb,b)   -> if a `myPairs` b && isJust ma && isJust mb then (case ma of {
                                                          Nothing -> ss+1;
                                                          Just x -> ss + 1}) else -888888 -- case ma of { Nothing -> whatever; Just x -> s.th}
  , unp = \ a ss b   -> if not (pairs a b) then ss else ss
  , h   = SM.foldl' max (-999998)
  }
{-# INLINE energyMin #-}

pretty :: Monad m => SigEnergyMin m [String] [[String]] Char (Maybe Char, Char) (Char, Maybe Char)
pretty = SigEnergyMin
  { ssr = \ [x] _  -> ["-" ++ x]
  , ssl = \ _ [x]  -> [x ++ "-"]
  , nil = \ ()     -> [""]
  , hl = \ _ [x] _ -> ["(" ++ x ++ ")"]
  , unp = \ _ [x] _ -> ["x" ++ x ++ "x"]
  , h   = SM.toList
  }
{-# INLINE pretty #-}

-- @TODO implement missing entropie parameters
-- @TODO ent_hl :: LoopSize -> Energy
ent_hl :: Int -> Double
ent_hl 3 = 4.1
ent_hl 4 = 4.9
ent_hl _ = error "not implemented yet"

-- how to check if SS is empty?
notEmpty :: Int -> Bool
notEmpty input
  = input == 0

-- Now, if you have custom behavior that you want to have for a certain set of types, then you have bounded polymorphism (also known as "ad hoc"). In Haskell we use type classes for this.
class MyPairs a b where
    myPairs :: a -> b -> Bool

instance MyPairs Char Char where
    myPairs a b = a `pairs` b

-- TODO instance MyPairs Just Just where
pairs :: Char -> Char -> Bool
pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
{-# INLINE pairs #-}

energyOf !a !b
  = 1
{-# INLINE energyOf #-}

energyMin :: Int -> String -> (Int,[[String]])
energyMin k inp = (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t) = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t)
{-# NOINLINE energyMinAlg #-}

type X = ITbl Id Unboxed Subword Int

chrRight :: VG.Vector v x => v x -> Chr (x, Maybe x) x
{-# Inline chrRight #-}
chrRight xs = Chr f xs where
  {-# Inline [0] f #-}
  f xs k = ( VG.unsafeIndex xs k
           , xs VG.!? (k+1)
           )

runInsideForward :: VU.Vector Char -> Z:.X
runInsideForward i = mutateTablesWithHints (Proxy :: Proxy CFG)
                   $ gEnergyMin energyMinAlg
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (chr i)
                        (chrLeft i)
                        (chrRight i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X -> [[String]]
runInsideBacktrack i (Z:.t) = unId $ axiom b
  where !(Z:.b) = gEnergyMin (energyMinAlg <|| pretty)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (chr i)
                          (chrLeft i)
                          (chrRight i)
{-# NoInline runInsideBacktrack #-}
