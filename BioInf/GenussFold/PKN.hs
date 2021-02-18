-- |

module Git.GenussFoldEnergyMin.BioInf.GenussFold.PKN where

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

import           FormalLanguage


-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: PKN
N: S
N: <X,2>
N: <Y,2>
N: <Z,2>
T: c
S: S

S -> unp <<< S c
-- S -> jux <<< S c S c
S -> nil <<< e
S -> khp <<< X Y X Z Y Z

--               pairs pairs parse       pairs pairs parse
<X,X> -> pk1 <<< [c,-] [c,-] [c,-] <X,X> [-,c] [-,c] [-,c]   -- will parse 6 nts
--                i1    i2     u          k2    k1
<X,X> -> nll <<< [e,e]

-- <W,W> -> pk1 <<< [c,-] <X,X> [-,c]
-- <W,W> -> pk1 <<< [c,-] <W,W> [-,c]

<Y,Y> -> pk2 <<< [S,-] [c,-] <Y,Y> [-,S] [-,c]   -- shall parse 4
<Y,Y> -> nll <<< [e,e]

<Z,Z> -> pk3 <<< [c,-] <Z,Z> [-,c]
<Z,Z> -> nll <<< [e,e]

//
Emit: PKN
|]

makeAlgebraProduct ''SigPKN

-- bpmax :: Monad m => SigPKN m Int Int Char Char -> In newer version 2D Terminal needs 2 types
bpmax :: Monad m => SigPKN m Int Int Char
bpmax = SigPKN
  { unp = \ x c     -> x
--  , jux = \ x c y d -> if c `pairs` d then x + y + 0 else -999999
  , khp = \ () () x () y z -> let m = minimum [x,y,z] in if m >= 1 then x + y +z else  -888888  -- iff one is zero than penalty
  , nil = \ ()      -> 0
  , pk1 = \ (Z:.i1:.()) (Z:.i2:.()) (Z:._:.()) y (Z:.():.k2) (Z:.():.k1) (Z:.():._) -> if i1 `pairs` k1 && i2 `pairs` k2 then y + 1 else -888888
  , pk2 = \ (Z:.x:.()) (Z:.a:.()) y (Z:.():.z) (Z:.():.b) -> if a `pairs` b then x + y + z + 1 else -888888
  , pk3 = \ (Z:.a:.()) y (Z:.():.b) -> if a `pairs` b then y + 1 else -888888
  , nll = \ (Z:.():.()) -> 0
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

pretty :: Monad m => SigPKN m [String] [[String]] Char
pretty = SigPKN
  { unp = \ [x] c     -> [x ++ "."]
--  , jux = \ [x] c [y] d -> [x ++ "(" ++ y ++ ")"]                                   -- x y x z y z
  , khp = \ () () [x1,x2] () [y1,y2] [z1,z2] -> [x1 ++ y1 ++ x2 ++ z1 ++ y2 ++ z2 ]   -- A B A C B C
  , nil = \ ()      -> [""]
--                                  pairs pairs parse       pairs pairs parse
  , pk1 = \ _ _ _ [y1,y2] _ _ _ -> ["((." ++ y1 , y2 ++ "))."]
  , pk2 = \ (Z:.[x]:.()) (Z:.a:.()) [y1,y2] (Z:.():.[z]) (Z:.():.b) -> [ x ++ "[" ++ y1 , y2 ++ z ++ "]"]
  , pk3 = \ (Z:.a:.()) [y1,y2] (Z:.():.b) -> ["(" ++ y1 , y2 ++ ")"]
  , nll = \ (Z:.():.()) -> ["",""]
  , h   = SM.toList
  }
{-# INLINE pretty #-}

pknPairMax :: Int -> String -> (Int,[[String]])
pknPairMax k inp = (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t:.u:.v:.w) = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t:.u:.v:.w)
{-# NOINLINE pknPairMax #-}

-- Tw ::
type X = ITbl Id Unboxed (Subword) Int
type T = ITbl Id Unboxed (Z:.Subword:.Subword) Int


runInsideForward :: VU.Vector Char -> Z:.X:.T:.T:.T
runInsideForward i = mutateTablesWithHints (Proxy :: Proxy MonotoneMCFG)
                   $ gPKN bpmax
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (-777999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (-888999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (-988999) []))
                        (chr i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

--type X' = BtITbl Unboxed (Subword) Int Id Id [String]
--type T' = BtITbl Unboxed (Z:.Subword:.Subword) Int Id Id [String]

runInsideBacktrack :: VU.Vector Char -> Z:.X:.T:.T:.T -> [[String]]
runInsideBacktrack i (Z:.t:.u:.v:.w) = unId $ axiom b
  where !(Z:.b:._:._:._) = gPKN (bpmax <|| pretty)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (toBacktrack u (undefined :: Id a -> Id a))
                          (toBacktrack v (undefined :: Id a -> Id a))
                          (toBacktrack w (undefined :: Id a -> Id a))
                          (chr i)
                      --    :: Z:.X':.T':.T'
{-# NoInline runInsideBacktrack #-}
