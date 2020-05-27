
module BioInf.GenussFold.EnergyMin where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List
import           Data.Vector.Fusion.Util
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Text.Printf

import           ADP.Fusion
import           Data.PrimitiveArray as PA hiding (map)

import           FormalLanguage

-- Rename to SimpleHairpinsNussinov

-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: EnergyMin
N: S
T: base
S: S
S -> singleStrandedL <<< base S
S -> singleStrandedR <<< S base
S -> hairpinLoop <<< base S base
S -> emptySequence <<< e

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin

energyMinAlg :: Monad m => SigEnergyMin m Int Int Char
energyMinAlg = SigEnergyMin
  { singleStrandedR = \ x c     -> x
  , singleStrandedL = \ c x     -> x
  , emptySequence = \ () -> 0
  , hairpinLoop = \ a ss b -> if a `pairs` b then ss + 1 else -888888
  , h   = SM.foldl' max (-999998)
  }
{-# INLINE energyMin #-}

pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
{-# INLINE pairs #-}


pretty :: Monad m => SigEnergyMin m [String] [[String]] Char
pretty = SigEnergyMin
  { singleStrandedR = \ [x] c     -> [x ++ "-"]
  , singleStrandedL = \ c [x]     -> ["-" ++ x]
  , emptySequence = \ () -> ["_"]
  , hairpinLoop = \ x [y] c-> ["(" ++ y ++ ")"]
  , h   = SM.toList
  }
{-# INLINE pretty #-}

energyMin :: Int -> String -> (Int,[[String]])
energyMin k inp = (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t) = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t)
{-# NOINLINE energyMinAlg #-}

type X = ITbl Id Unboxed Subword Int

runInsideForward :: VU.Vector Char -> Z:.X
runInsideForward i = mutateTablesWithHints (Proxy :: Proxy CFG)
                   $ gEnergyMin energyMinAlg
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (chr i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X -> [[String]]
runInsideBacktrack i (Z:.t) = unId $ axiom b
  where !(Z:.b) = gEnergyMin (energyMinAlg <|| pretty)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (chr i)
{-# NoInline runInsideBacktrack #-}
