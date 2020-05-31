
module BioInf.GenussFold.SimpleEnergyStackedHairpinLoops where

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

-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: SimpleEnergyStackedHairpinLoops
N: S
T: nt
S: S
S -> ssl <<< nt S
S -> ssr <<< S nt
S -> stackedHL <<< nt nt S nt nt
S -> emptySequence <<< e

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin

energyMinAlg :: Monad m => SigEnergyMin m Int Int Char
energyMinAlg = SigEnergyMin
  { ssr = \ x c     -> x
  , ssl = \ c x     -> x
  , emptySequence = \ () -> 0
  , stackedHL = \ a b ss c d -> if b `pairs` c && a `pairs` d then ss + 1 else -888888
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
  { ssr = \ [x] c     -> [x ++ "-"]
  , ssl = \ c [x]     -> ["-" ++ x]
  , emptySequence = \ () -> ["_"]
  , stackedHL = \ a b [y] c d-> ["(" ++ y ++ ")"]
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
