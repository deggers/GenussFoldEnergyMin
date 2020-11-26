
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
import           Text.Printf

import           ADP.Fusion
import           Data.PrimitiveArray as PA hiding (map)

import           FormalLanguage



-- | Define signature and grammar
-- Simple G5 from Ivo Hofacker S -> aS | aSaS | e
[formalLanguage|
Verbose

Grammar: PKN
N: S
T: nt
S: S
S -> unp <<< nt S
S -> prs <<< nt S nt S
S -> nil <<< e

//
Emit: PKN
|]

makeAlgebraProduct ''SigPKN

-- | Evaluation algebra
bpmax :: Monad m => SigPKN m Int Int Char
bpmax = SigPKN
  { unp = \ c x     -> x
  , prs = \ a x b y -> if a `pairs` b then x + y + 1 else -999999
  , nil = \ ()      -> 0
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

-- |
--
-- TODO It could be beneficial to introduce
-- @type Splitted = Either String (String,String)@
-- or something isomorphic. While [String] works, it allows for too many
-- possibilities here! ([] ist lightweight, on the other hand ...)

-- Evaluation Algebra
pretty :: Monad m => SigPKN m [String] [[String]] Char
pretty = SigPKN
  { unp = \ c [x]     -> ["-" ++ x]
  , prs = \ a [x] b [y] -> [ "(" ++ x ++ ")" ++ y]
  , nil = \ ()      -> [""]
  , h   = SM.toList
  }
{-# INLINE pretty #-}

-- |
--
-- @
-- [{]}(())
-- caguagcu
-- [ ]
--  { }
--     (())
-- @

g5Max :: Int -> String -> (Int,[[String]])
g5Max k inp = (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  !(Z:.t) = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t)
{-# NOINLINE g5Max #-}

type X = ITbl Id Unboxed Subword Int

runInsideForward :: VU.Vector Char -> Z:.X
runInsideForward i = mutateTablesWithHints (Proxy :: Proxy MonotoneMCFG)
                   $ gPKN bpmax
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (chr i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X -> [[String]]
runInsideBacktrack i (Z:.t) = unId $ axiom b
  where !(Z:.b) = gPKN (bpmax <|| pretty)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (chr i)
{-# NoInline runInsideBacktrack #-}
