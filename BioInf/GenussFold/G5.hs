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
T: regionCtx
S: S
S -> unp <<< nt S
S -> prs <<< regionCtx S regionCtx
S -> nil <<< e

//
Emit: G5
|]

makeAlgebraProduct ''SigG5
type Pos = Int
type Nt = Char
type Basepair = (Nt, Nt)

interiorLoopEnergy ::  Basepair -> Basepair -> Double
interiorLoopEnergy ('C','G') ('G','U') = 1.40
interiorLoopEnergy ('G','U') ('C','G') = 2.50
interiorLoopEnergy ('C','G') ('U','A') = 2.10
interiorLoopEnergy ('U','A') ('U','A') = 0.90
interiorLoopEnergy ('C','G') ('A','U') = 2.10
interiorLoopEnergy ('A','U') ('U','A') = 1.10
interiorLoopEnergy ('U','A') ('A','U') = 1.30
interiorLoopEnergy ('U','A') ('C','G') = 2.40
interiorLoopEnergy ('C','G') ('C','G') = 3.30
interiorLoopEnergy ('C','G') ('U','G') = 2.10
interiorLoopEnergy ('U','G') ('A','U') = 1.00
interiorLoopEnergy ('A','U') ('A','U') = 0.90
interiorLoopEnergy ('G','C') ('A','U') = 2.40
interiorLoopEnergy ('A','U') ('G','C') = 2.10
interiorLoopEnergy _ _ = 1.51

-- | Evaluation algebra
bpmax :: Monad m => VU.Vector Char -> SigG5 m Double Double Char (Pos,Pos)
bpmax input = SigG5
  { unp = \ _ x     -> x - 0.2
  , prs = \ (i,j) a (subtract 1 -> k, subtract 1 -> l) -> if
        | pairs (input VU.! i) (input VU.! l) && pairs (input VU.! j) (input VU.! k) ->
            a + interiorLoopEnergy (input VU.! i, input VU.! l) (input VU.! j, input VU.! k) -- interior loop case
        | (l-i > 2) && (j-i == 1) && (l-k == 1) && pairs (input VU.! i) (input VU.! l)  && (not $ pairs (input VU.! j) (input VU.! k)) ->
            5 -- hairpin case
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

-- |
--
-- TODO It could be beneficial to introduce
-- @type Splitted = Either String (String,String)@
-- or something isomorphic. While [String] works, it allows for too many
-- possibilities here! ([] ist lightweight, on the other hand ...)

-- Evaluation Algebra
pretty :: Monad m => VU.Vector Char -> SigG5 m [String] [[String]] Char (Pos,Pos)
pretty input = SigG5
  { unp = \ _ [x]     -> ["." ++ x]
--  , prs = \ (i,j) [x] (k,l) [y] -> [ "(" ++ replicate (j-i) '.' ++ x ++ ")" ++ replicate (l-k) '.' ++  y]
  , prs = \ (i,subtract 1 ->j) [a] (k, subtract 1 -> l) -> if
        | (k-j > 2) && (j-i == 1) && (l-k == 1) && pairs (input VU.! i) (input VU.! l) && pairs (input VU.! j) (input VU.! k) ->
            [ "((" ++ replicate (j-i) '.' ++ a ++ "))" ++ replicate (l-k) '.' ]
        | (k-j > 2) && pairs (input VU.! i) (input VU.! l) ->
            [ "(" ++ replicate (j-i) '.' ++ a ++ ")" ++ replicate (l-k) '.' ]
        | otherwise -> error "should not happen!"
  , nil = \ ()      -> [""]
  , h   = SM.toList
  }
{-# INLINE pretty #-}

prettyStruct :: Monad m => VU.Vector Char -> SigG5 m [String] [[String]] Char (Pos,Pos)
prettyStruct input = SigG5
  { unp = \ _ [x]     -> ["Unpaired " ++ x]
  , prs = \ (i, j) [a] (subtract 1 -> k, subtract 1 -> l) -> if
        |  pairs (input VU.! i) (input VU.! l) && pairs (input VU.! j) (input VU.! k) ->
            [ "Interior Loop " ++ "(i:" ++ show i ++ ", j:" ++ show j ++  ") (k:" ++ show k ++ ",l:" ++ show l ++ ") " ++ a  ]
        |  (l-i > 2) && pairs (input VU.! i) (input VU.! l)  && (not $ pairs (input VU.! j) (input VU.! k)) ->
            [ "Hairpin (i:" ++ show i ++ ", j:" ++ show j ++  ") (k:" ++ show k ++ ",l:" ++ show l ++ ") " ]
        | otherwise -> error "should not happen!"
  , nil = \ ()      -> ["Nil"]
  , h   = SM.toList
  }
{-# INLINE prettyStruct #-}

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
                        (chr i)
                        (idxStrng 1 31 i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X -> [[String]]
runInsideBacktrack i (Z:.t) = unId $ axiom b
  where !(Z:.b) = gG5 (bpmax i <|| prettyStruct i)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (chr i)
                          (idxStrng 1 31 i)
{-# NoInline runInsideBacktrack #-}
