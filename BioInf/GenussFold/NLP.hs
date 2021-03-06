{-# Language GeneralizedNewtypeDeriving #-}

module BioInf.GenussFold.NLP where

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

import           BioInf.GenussFold.Grammars.NLP

-- bpmax :: Monad m => SigPKN m Int Int Char Char -> In newer version 2D Terminal needs 2 types
bpmax :: Monad m => Int -> SigPKN m Int Int Char
bpmax p = SigPKN
  { unp = \ _ x
    -> x
  , jux = \ a x b y
    -> if a `pairs` b then x + y + 1 else -999999
  , pkn = \ () () x y
    -> let m = minimum [x,y] in if m >= 1 then x + y - p else -888888
  , tmp = \ t u
    -> t + u
  , nil = \ ()
    -> 0
  , pk1 = \ (Z:.():.a) (Z:.():.m) y (Z:.b:.()) (Z:.n:.())
    -> if a `pairs` b then m + n + y + 1 else -888888
  , pk1b = \ (Z:.a:.()) (Z:.():.b) (Z:.s1:.()) (Z:.():.s2)
    -> if a `pairs` b then s1 + s2 + 1 else -88888
  , pk2 = \ (Z:.():.a) (Z:.():.m) y (Z:.b:.()) (Z:.n:.())
    -> if a `pairs` b then m + n + y + 1 else -888888
  , pk2b = \ (Z:.a:.()) (Z:.():.b) (Z:.s1:.()) (Z:.():.s2)
    -> if a `pairs` b then s1 + s2 + 1 else -88888
  , h   = SM.foldl' max (-999999)
  }
{-# INLINE bpmax #-}

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
  , tmp = \ [t] [u]
    -> [t ++ u]
  , nil = \ ()
    -> [""]
  , pk1 = \ _ (Z:.():.[t2]) [x1,x2] _ (Z:.[t1]:.())
    -> [ x1 ++ "{" ++ t1 , "}" ++  t2 ++ x2]
  , pk1b = \ _ _ (Z:.[t1]:.()) (Z:.():.[t2])
    -> ["{" ++ t1 , "}" ++ t2]
  , pk2 = \ _ (Z:.():.[s2]) [x1,x2] _ (Z:.[s1]:.())
    -> [ x1 ++ "[" ++ s1 , "]" ++  s2 ++ x2]
  , pk2b = \ _ _ (Z:.[t1]:.()) (Z:.():.[t2])
    -> ["[" ++ t1 , "]" ++ t2]
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
  !(Z:.t:.u:.v:.w:.x) = runInsideForward i p
  d = unId $ axiom t
  bs = runInsideBacktrack i p (Z:.t:.u:.v:.w:.x)
{-# NOINLINE energyMin #-}

type X = ITbl Id Unboxed (Subword) Int
type T = ITbl Id Unboxed (Z:.Subword:.Subword) Int

runInsideForward :: VU.Vector Char -> Int -> Z:.X:.X:.X:.T:.T
runInsideForward i p = mutateTablesWithHints (Proxy :: Proxy MonotoneMCFG)
                   $ gPKN (bpmax p)
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (ITbl 0 1 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (ITbl 0 2 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (-777999) []))
                        (ITbl 0 1 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (-888999) []))
                        (chr i)
  where n = VU.length i
{-# NoInline runInsideForward #-}


runInsideBacktrack :: VU.Vector Char -> Int -> Z:.X:.X:.X:.T:.T -> [[String]]
runInsideBacktrack i p (Z:.t:.u:.v:.w:.x) = unId $ axiom b
  where !(Z:.b:._:._:._:._) = gPKN (bpmax p <|| pretty)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (toBacktrack u (undefined :: Id a -> Id a))
                          (toBacktrack v (undefined :: Id a -> Id a))
                          (toBacktrack w (undefined :: Id a -> Id a))
                          (toBacktrack x (undefined :: Id a -> Id a))
                          (chr i)
                      --    :: Z:.X':.T':.T'
{-# NoInline runInsideBacktrack #-}
