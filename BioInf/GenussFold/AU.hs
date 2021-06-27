{-# Language GeneralizedNewtypeDeriving #-}

module BioInf.GenussFold.AU where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List
import           Data.Vector.Fusion.Util
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Text.Printf

import           ADP.Fusion
import           ADP.Fusion.Base.Subword
import           Data.PrimitiveArray as PA hiding (map)
import           ADP.Fusion.SynVar.Fill

import           BioInf.GenussFold.Grammars.AU

-- algebra :: Monad m => SigPKN m Int Int Char Char -> In newer version 2D Terminal needs 2 types
algebra :: Monad m => Int -> SigPKN m Int Int Char
algebra p = SigPKN
  { unp = \ _ x
    -> x
  , pkn = \ () x y
    -> let m = minimum [x] in if m >= 1 then x - p else -888888
  , nil = \ ()
    -> 0
  , pk1 = \ (Z:.():.a) (Z:.():.z) x (Z:.b:.()) (Z:.y:.())
    -> if a `pairs` b then y + x + z + 1 else -888888
  , pk1b = \ (Z:.a:.()) (Z:.():.b) (Z:.s1:.()) (Z:.():.s2)
    -> if a `pairs` b then s1 + s2 + 1 else -88888
  , pk2 = \ (Z:.():.a) (Z:.():.z) x (Z:.b:.()) (Z:.y:.())
    -> if a `pairs` b then y + x + z + 1 else -888888
  , pk2b = \ (Z:.a:.()) (Z:.():.b) (Z:.s1:.()) (Z:.():.s2)
    -> if a `pairs` b then s1 + s2 + 1 else -88888
  , h   = SM.foldl' max (-999999)
  }
{-# INLINE algebra #-}

pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
{-# INLINE pairs #-}

pretty :: Monad m => SigPKN m [String] [[String]] Char
pretty = SigPKN
  { unp = \ _ [x]
    -> ["." ++ x]
  , jux = \ _ [x] _ [y]
    -> ["(" ++ x ++ ")" ++ y]
  , pkn = \ () () [x1,x2] [y1,y2]
    -> [x1 ++ y1 ++ x2 ++ y2]
  , nil = \ ()
    -> [""]
  , pk1 = \ (Z:.():._) (Z:.():.[s2]) [x1,x2] (Z:._:.()) (Z:.[s1]:.())
    -> [ x1 ++ "{" ++ s1, "}" ++ s2 ++ x2]
--     [ topSide, bottomSide ]
  , pk1b = \ _ _ (Z:.[s1]:.()) (Z:.():.[s2])
    -> ["{" ++ s1 , "}" ++ s2]
  , pk2 = \ _ (Z:.():.[s2]) [x1,x2] _ (Z:.[s1]:.())
    -> [x1 ++ "[" ++ s1, "]" ++ s2 ++ x2]
  , pk2b = \ _ _ (Z:.[s1]:.()) (Z:.():.[s2])
    -> ["[" ++ s1 , "]" ++ s2]
  , h   = SM.toList
  }
{-# INLINE pretty #-}

-- The number of desired backtracks
newtype NumBT = NumBT Int -- alias for an Int :: But newtype as explicit naming
  deriving Num

-- The penalty for opening a pseudoknot
newtype PenPK = PenPK Int
  deriving Num

energyMin :: NumBT -> PenPK -> String -> (Int,[[String]])
energyMin (NumBT k) (PenPK p) inp = (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t:.u:.v) = runInsideForward i p
  d = unId $ axiom t
  bs = runInsideBacktrack i p (Z:.t:.u:.v)
{-# NOINLINE energyMin #-}

type X = ITbl Id Unboxed (Subword) Int
type T = ITbl Id Unboxed (Z:.Subword:.Subword) Int

runInsideForward :: VU.Vector Char -> Int -> Z:.X:.T:.T
runInsideForward i p = mutateTablesWithHints (Proxy :: Proxy MonotoneMCFG)
                   $ gPKN (algebra p)
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (-777999) []))
                        (ITbl 0 1 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (-888999) []))
                        (chr i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Int -> Z:.X:.T:.T -> [[String]]
runInsideBacktrack i p (Z:.t:.u:.v) = unId $ axiom b
  where !(Z:.b:._:._) = gPKN (algebra p <|| pretty)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (toBacktrack u (undefined :: Id a -> Id a))
                          (toBacktrack v (undefined :: Id a -> Id a))
                          (chr i)
                      --    :: Z:.X':.T':.T'
{-# NoInline runInsideBacktrack #-}
