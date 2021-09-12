{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}

{-# Options_GHC -fspec-constr-count=1000 #-}
{-# Options_GHC -fspec-constr-recursive=1000 #-}
{-# Options_GHC -fspec-constr-threshold=1000 #-}
{-# Options_GHC -fmax-worker-args=1000 #-} -- temporäre funktionen bis zu 1000 args ok
{-# Options_GHC -flate-dmd-anal #-} -- gibts wahrscheinlich nicht in ghci 7 - late demand analys
-- both, full laziness and no liberate case are essential to have things inline nicely!
{-# Options_GHC -fno-full-laziness #-} -- stellt sicher das keine argument rausgeworfen werden könnte probleme mit Fusion erzeugen
{-# Options_GHC -fno-liberate-case #-} -- alle cases betrachten

module BioInf.GenussFold.ViennaRNA_RN where


import           Debug.Trace
import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Control.DeepSeq
import           Data.Char (toUpper,toLower,isSpace)
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
import           Foreign.Ptr
import           System.IO.Unsafe

import           ADP.Fusion
import           Data.PrimitiveArray as PA hiding (map)

import           FormalLanguage

import           BioInf.GenussFold.IndexToIndexParser
import           BioInf.GenussFold.IdxStrng
import qualified BioInf.ViennaRNA.Bindings.Inline as V
import qualified Data.ByteString.Char8 as BS
import           GHC.Float

import           BioInf.GenussFold.Grammars.ViennaRNA_RN

-- Use domain-specific language
type Pos = Int
type Nt = Char
type NtPos = (Nt, Pos)
type Basepair = (Nt, Nt)
type Energy = Int
type IndexRegionParser = (Pos,Pos)
ignore      = 100123

energyMinAlg :: Monad m => BS.ByteString -> Int -> Ptr() -> SigEnergyMin m Energy Energy NtPos (Pos,Pos)
energyMinAlg input penalty compound = SigEnergyMin
  { nil  = \ () -> 0
  , hpkn = \ () () a b          --  :: A B A B
    -> let m = maximum [a,b] in if m < 0 then a + b + penalty else ignore
   -- interiorLoops instead of stacking
  , mpkn = \ () () a () b c       -- :: A C A B C B
    -> let m = maximum [a,b,c] in if m < 0 then a + b + c + penalty else ignore
  , lpkn = \ () () () a b c       -- :: A B C A B C
    -> let m = maximum [a,b,c] in if m < 0 then a + b + c + penalty else ignore
  , kpkn = \ () () () a () b c d  -- :: A B C A D B C D
    -> let m = maximum [a,b,c,d] in if m < 0 then a + b + c + d + penalty else ignore
  , pk1a = \ (Z:.():.(lPos,jPos)) (Z:.():.m) y (Z:.(subtract 1 -> iPos, subtract 1 -> kPos):.()) (Z:.n:.()) -> if
      | (minimum [lPos,jPos,iPos,kPos] >= 0)
        && (jPos < BS.length input)
        && pairs (BS.index input iPos) (BS.index input jPos)
        && pairs (BS.index input kPos) (BS.index input lPos)
       -- -> traceShow ("pk1" ++ show (iPos,jPos,kPos,lPos)) $ m + n + y + evalIP input iPos jPos kPos lPos
       -> m + n + y + evalIP compound iPos jPos kPos lPos
      | otherwise -> ignore
  , pk2a = \ (Z:.():.(lPos,jPos)) (Z:.():.m) y (Z:.(subtract 1 -> iPos, subtract 1 -> kPos):.()) (Z:.n:.()) -> if
      | (minimum [lPos,jPos,iPos,kPos] >= 0)
        && (jPos < BS.length input)
        && pairs (BS.index input iPos) (BS.index input jPos)
        && pairs (BS.index input kPos) (BS.index input lPos)
        -> m + n + y + evalIP compound iPos jPos kPos lPos
      | otherwise -> ignore
  , pk3a = \ (Z:.():.(lPos,jPos)) (Z:.():.m) y (Z:.(subtract 1 -> iPos, subtract 1 -> kPos):.()) (Z:.n:.()) -> if
      | (minimum [lPos,jPos,iPos,kPos] >= 0)
        && (jPos < BS.length input)
        && pairs (BS.index input iPos) (BS.index input jPos)
        && pairs (BS.index input kPos) (BS.index input lPos)
        -> m + n + y + evalIP compound iPos jPos kPos lPos
      | otherwise -> ignore
  , pk4a = \ (Z:.():.(lPos,jPos)) (Z:.():.m) y (Z:.(subtract 1 -> iPos, subtract 1 -> kPos):.()) (Z:.n:.()) -> if
      | (minimum [lPos,jPos,iPos,kPos] >= 0)
        && (jPos < BS.length input)
        && pairs (BS.index input iPos) (BS.index input jPos)
        && pairs (BS.index input kPos) (BS.index input lPos)
        -> m + n + y + evalIP compound iPos jPos kPos lPos
      | otherwise -> ignore
  , pk1b = \ (Z:.(i,iPos):.()) (Z:.():.(j,jPos)) (Z:.s1:.()) (Z:.():.s2) -> if
      | pairs i j
        -> s1 + s2 -- - 330
      --  -> traceShow ("pk1b" ++ show(i,iPos,j,jPos)) $ s1 + s2 - 330
      | otherwise -> ignore
  , pk2b = \ (Z:.(i,iPos):.()) (Z:.():.(j,jPos)) (Z:.s1:.()) (Z:.():.s2) -> if
      | pairs i j
        ->  s1 + s2 -- - 330
      | otherwise -> ignore
  , pk3b = \ (Z:.(i,iPos):.()) (Z:.():.(j,jPos)) (Z:.s1:.()) (Z:.():.s2) -> if
      | pairs i j
        ->  s1 + s2 -- - 330
      | otherwise -> ignore
  , pk4b = \ (Z:.(i,iPos):.()) (Z:.():.(j,jPos)) (Z:.s1:.()) (Z:.():.s2) -> if
      | pairs i j
        ->  s1 + s2 -- - 330
      | otherwise -> ignore
  , unp = \ c ss ->  ss
  , jux   = \ x y -> x + y -- traceShow ("JXP" ++ show (x,y)) $ x + y
  , hairpin  = \ (iPos, subtract 1 -> jPos) -> if
    | (jPos-iPos) > 3 && pairs (BS.index input iPos) (BS.index input jPos)
      -> (evalHP compound iPos jPos)
  -- -> traceShow ("HP" ++ show (iPos,jPos, evalHP input iPos jPos)) $ evalHP input iPos jPos
    | otherwise -> ignore
  , interior = \ (iPos, kPos) closed (subtract 1 -> lPos, subtract 1 -> jPos) -> let e = evalIP compound iPos jPos kPos lPos in if
    | pairs (BS.index input iPos) (BS.index input jPos)
      && pairs (BS.index input kPos) (BS.index input lPos)
      -> closed + evalIP compound iPos jPos kPos lPos
      -- -> traceShow ("INT " ++ show (closed,iPos,jPos,kPos,lPos,e)) $ e + closed -- evalIP input iPos jPos kPos lPos  --subtract 330 closed -- interiorLoopEnergy (BS.index input iPos, BS.index input jPos) (BS.index input kPos, BS.index input lPos)
    | otherwise -> ignore
  , mlr      = \ (a,_) m m1 (d,_) -> if
             | pairs a d -> m + m1 + 290 -- NOTE :: Just an approximation, not accurate - but accurate enough
             | otherwise   -> ignore
  , mcm_1 = \ _ closed -> closed
  , mcm_2 = \  m closed ->  m + closed
  , mcm_3 = \  m _ ->  m
  , ocm_1 = \  m1 -> m1
  , ocm_2 = \ closed _ -> closed
  , h    =   SM.foldl' min (ignore)
  }
{-# INLINE energyMinAlg #-}

data Pretty = Pnil | Ppkn [Pretty]
  deriving Show

pretty :: Monad m => SigEnergyMin m [String] [[String]] NtPos (Pos,Pos)
pretty = SigEnergyMin
  { nil = \ () ->  [""]
  , hpkn = \ () () [a1,a2] [b1,b2]
    -> [a1 ++ b1 ++ a2 ++ b2]
  , mpkn = \ () () [a1,a2] () [c1,c2] [b1,b2]              -- :: A C A B C B
    -> [a1 ++ c1 ++ a2 ++ b1 ++ c2 ++ b2]
  , lpkn = \ () () () [a1,a2] [b1,b2] [c1,c2]              -- :: A B C A B C
    -> [a1 ++ b1 ++ c1 ++ a2 ++ b2 ++ c2]
  , kpkn = \ () () () [a1,a2] () [b1,b2] [c1,c2] [d1,d2]   -- :: A B C A D B C D
    -> [a1 ++ b1 ++ c1 ++ a2 ++ d1 ++ b2 ++ c2 ++ d2]
  , pk1a = \ (Z:.():.(lPos,jPos)) (Z:.():.[s2]) [x1,x2] (Z:.(iPos,kPos):.()) (Z:.[s1]:.())
    -> [ x1 ++ replicate (kPos-iPos-1) '.' ++ "{" ++ s1 , "}" ++ replicate (jPos-lPos-1) '.' ++ x2 ++ s2]
  , pk2a = \ (Z:.():.(lPos,jPos)) (Z:.():.[s2]) [x1,x2] (Z:.(iPos,kPos):.()) (Z:.[s1]:.())
    -> [ x1 ++ replicate (kPos-iPos-1) '.' ++ "[" ++ s1 , "]" ++ replicate (jPos-lPos-1) '.' ++ x2 ++ s2]
  , pk3a = \ (Z:.():.(lPos,jPos)) (Z:.():.[s2]) [x1,x2] (Z:.(iPos,kPos):.()) (Z:.[s1]:.())
    -> [ x1 ++ replicate (kPos-iPos-1) '.' ++ "a" ++ s1 , "A" ++ replicate (jPos-lPos-1) '.' ++ x2 ++ s2]
  , pk4a = \ (Z:.():.(lPos,jPos)) (Z:.():.[s2]) [x1,x2] (Z:.(iPos,kPos):.()) (Z:.[s1]:.())
    -> [ x1 ++ replicate (kPos-iPos-1) '.' ++ "b" ++ s1 , "B" ++ replicate (jPos-lPos-1) '.' ++ x2 ++ s2]
  , pk1b = \ (Z:.(l,lPos):.()) (Z:.():.(r,rPos)) (Z:.[t1]:.()) (Z:.():.[t2])
    -> ["{" ++ t1 , "}" ++ t2]
  , pk2b = \ (Z:.(l,lPos):.()) (Z:.():.(r,rPos)) (Z:.[t1]:.()) (Z:.():.[t2])
    -> ["[" ++ t1 , "]" ++ t2]
  , pk3b = \ (Z:.(l,lPos):.()) (Z:.():.(r,rPos)) (Z:.[t1]:.()) (Z:.():.[t2])
    -> ["a" ++ t1 , "A" ++ t2]
  , pk4b = \ (Z:.(l,lPos):.()) (Z:.():.(r,rPos)) (Z:.[t1]:.()) (Z:.():.[t2])
    -> ["b" ++ t1 , "B" ++ t2]
  , unp = \ _ [ss] -> ["." ++ ss]
  , jux = \ [x] [y] -> [x ++ y]
  , hairpin = \  (iPos,subtract 1 -> jPos)  -> ["(" ++ replicate (jPos-iPos-1) '.' ++ ")"]
  , interior = \ (iPos, kPos) [closed] (subtract 1 -> lPos, subtract 1 -> jPos)
    -> ["(" ++ replicate (kPos-iPos-1) '.' ++ closed ++ replicate (jPos-lPos-1) '.' ++ ")"]
  , mlr = \ _ [m] [m1] _ -> ["(" ++ m ++ m1 ++ ")"]
  , mcm_1 = \ (iPos,  jPos) [closed] -> [replicate (jPos-iPos) '.' ++ closed ]
  , mcm_2 = \ [m] [closed] -> [m ++ closed ]
  , mcm_3 = \ [m] _ -> [m ++ "."]
  , ocm_1 = \ [m1] -> [m1]
  , ocm_2 = \ [x] _ -> [x ++ "."]
  , h   = SM.toList
  }
{-# INLINE pretty #-}

-- The number of desired backtracks
newtype NumBT = NumBT Int -- alias for an Int :: But newtype as explicit naming
  deriving (Num, Show, Read)

-- The penalty for opening a pseudoknot
newtype PenPK = PenPK Int
  deriving Num

instance Read PenPK where
 readsPrec p s = [ (PenPK i, t) | (i,t) <- readsPrec p s ]

instance Show PenPK where
  show (PenPK p) = show p

energyMin :: NumBT -> PenPK -> String -> (Energy,[[String]])
energyMin (NumBT k) (PenPK p) inp = unsafePerformIO $ do
  let
    i = BS.pack . Prelude.map toUpper $ inp
  c <- V.mkFoldCompound i
  let
    iv = VU.fromList . Prelude.map toUpper $ inp
--       1  2  3  4  1  2  3  4
    !(Z:.a:.b:.e:.f:.l:.r:.s:.t) = runInsideForward i iv p c
    z = unId $ axiom a -- gets the value from the table
--                                       1  2  3  4  1  2  3  4
    bs = take k $ runInsideBacktrack i iv p c (Z:.a:.b:.e:.f:.l:.r:.s:.t)
  deepseq bs $ V.destroyFoldCompound c
  return (z, bs)
{-# NOINLINE energyMin #-}

type X = ITbl Id Unboxed Subword Energy
type T = ITbl Id Unboxed (Z:.Subword:.Subword) Energy
--                                                                        1  2  3  4  1  2  3  4
runInsideForward :: BS.ByteString -> VU.Vector Char -> Int -> Ptr() -> Z:.X:.X:.X:.X:.T:.T:.T:.T
runInsideForward i iv p c = mutateTablesWithHints (Proxy :: Proxy MonotoneMCFG)
                   $ gEnergyMin (energyMinAlg i p c)
                        (ITbl 0 1 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (166999) [])) -- 1
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (266999) [])) -- 2
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (366999) [])) -- 3
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (466999) [])) -- 4
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (777999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (888999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (888999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (888999) []))
                        (ntPos iv)
                        (idxStrng1 1 31 iv)
  where n = BS.length i
{-# NoInline runInsideForward #-}
--                                                                          1  2  3  4  1  2  3  4
runInsideBacktrack :: BS.ByteString -> VU.Vector Char -> Int -> Ptr() -> Z:.X:.X:.X:.X:.T:.T:.T:.T -> [[String]] -- for the non-terminals:.T:.T
--                              1  2  3  4  1  2  3  4
runInsideBacktrack i iv p c (Z:.a:.b:.e:.f:.s:.t:.u:.v) = unId $ axiom g -- Axiom from the Start Nonterminal S -> a_Struct-
--           1  2  3  4  1  2  3  4
  where !(Z:.g:._:._:._:._:._:._:._) = gEnergyMin (energyMinAlg i p c<|| pretty)
  -- where !(Z:.g:.h:.l:.m:.n:.o:.p) = gEnergyMin (energyMinAlg i <|| prettyPaths i)
                          (toBacktrack a (undefined :: Id y -> Id y)) -- 1
                          (toBacktrack b (undefined :: Id y -> Id y)) -- 2
                          (toBacktrack e (undefined :: Id y -> Id y)) -- 3
                          (toBacktrack f (undefined :: Id y -> Id y)) -- 4
                          (toBacktrack s (undefined :: Id y -> Id y)) -- 1
                          (toBacktrack t (undefined :: Id y -> Id y)) -- 2
                          (toBacktrack u (undefined :: Id y -> Id y)) -- 3
                          (toBacktrack v (undefined :: Id y -> Id y)) -- 4
                          (ntPos iv)
                          (idxStrng1 1 31 iv)
{-# NoInline runInsideBacktrack #-}

pairs :: Char -> Char -> Bool
pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
{-# INLINE pairs #-}


ntPos :: VG.Vector v x => v x -> Chr (x,Int) x
{-# Inline ntPos #-}
ntPos xs = Chr f xs where
  {-# Inline [0] f #-}
  f xs k = (VG.unsafeIndex xs k, k)

evalIP :: Ptr() -> Int -> Int -> Int -> Int -> Energy
evalIP  c ((+1 ) -> i) ( (+1) -> j) ((+1) -> k) ((+1) -> l) = V.intLoopCP c i j k l

evalHP :: Ptr() -> Int -> Int -> Energy
evalHP c ((+1) -> i) ((+1) -> j) = V.hairpinCP c i j
