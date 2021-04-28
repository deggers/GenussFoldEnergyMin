{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module BioInf.GenussFold.G5 where

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
import qualified Data.Vector.Generic as VG
import           Text.Printf

import           ADP.Fusion
import           Data.PrimitiveArray as PA hiding (map)

import           BioInf.GenussFold.IndexToIndexParser
import           BioInf.GenussFold.IdxStrng

import           FormalLanguage

-- | Define signature and grammar
-- Simple G5 from Ivo Hofacker S -> aS | aSaS | e
[formalLanguage|
Verbose

Grammar: G5
N: S
T: nt
S: S
S -> unp <<< nt S
S -> khp <<< nt nt nt nt nt S nt nt nt nt nt S
S -> nil <<< e

//
Emit: G5
|]

makeAlgebraProduct ''SigG5
type Pos = Int
type Nt = Char
type Basepair = (Nt, Nt)

-- | Evaluation algebra
bpmax :: Monad m => VU.Vector Char -> SigG5 m Double Double (Char,Int)
bpmax input = SigG5
  { unp = \ _ x     -> x
  , khp = \ (a,aPos) (b,bPos) (c,cPos) (d,dPos) (a',a'Pos) x (e,ePos) (f,fPos) (g,gPos) (h,hPos) (e',e'Pos) y -> if
        | (a'Pos-aPos > 3) && pairs a a' && (e'Pos-ePos > 3) && pairs e e' && pairs b f && pairs c g && pairs d h -> 1 + x + y
        | otherwise -> -9999999.00
  , nil = \ ()      -> 0
  , h   = SM.foldl' max (-999999.00)
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

-- Evaluation Algebra
pretty :: Monad m => VU.Vector Char -> SigG5 m [String] [[String]] (Char,Int)
pretty input = SigG5
  { unp = \ _ [x]     -> ["." ++ x]
--  , prs = \ (i,j) [x] (k,l) [y] -> [ "(" ++ replicate (j-i) '.' ++ x ++ ")" ++ replicate (l-k) '.' ++  y]
  , khp = \ _ _ _ _ _ [a] _ _ _ _ _ [b] -> ["([[[)" ++ a ++ "(]]])" ++ b]
  , nil = \ ()      -> [""]
  , h   = SM.toList
  }
{-# INLINE pretty #-}

g5Max :: Int -> String -> (Double,[[String]])
g5Max k inp = (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  !(Z:.t) = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t)
{-# NOINLINE g5Max #-}

type X = ITbl Id Unboxed Subword Double

--runInsideForward :: VU.Vector Char -> Z:.X
runInsideForward i = mutateTablesWithHints (Proxy :: Proxy CFG)
                   $ gG5 (bpmax i)
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (ntPos i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X -> [[String]]
runInsideBacktrack i (Z:.t) = unId $ axiom b
  where !(Z:.b) = gG5 (bpmax i <|| pretty i)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (ntPos i)
{-# NoInline runInsideBacktrack #-}

ntPos :: VG.Vector v x => v x -> Chr (x,Int) x
{-# Inline ntPos #-}
ntPos xs = Chr f xs where
  {-# Inline [0] f #-}
  f xs k = (VG.unsafeIndex xs k, k)
