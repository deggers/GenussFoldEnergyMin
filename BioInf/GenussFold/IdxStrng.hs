-- |
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BioInf.GenussFold.IdxStrng where

import           Data.Strict.Tuple
import qualified Data.Vector.Generic as VG
import           Data.PrimitiveArray
import           ADP.Fusion.Base

-- | 'Strng' terminals return "strings", i.e. vectors of @Chr@s. They allow
-- the user to specify @[ 0 .. ]@ atoms to be parsed at once. It is
-- possible to both, limit the minimal and maximal number.
--
-- NOTE gadt comments are not parsed by haddock?

--  Returns pair of indices, where first pair is withing region and second is outside of region
-- if indices are the same, its an empty region beginning from first pair
data IdxStrng v x where
 IdxStrng :: VG.Vector v x
        => Int                         -- minimal size
        -> Int                         -- maximal size (just use s.th. big if you don't want a limit)
        -> (v x)                       -- the actual vector
        -> IdxStrng v x

idxStrng :: VG.Vector v x => Int -> Int -> v x -> IdxStrng v x
idxStrng = \minL maxL xs -> IdxStrng (max minL 0) maxL xs
{-# Inline idxStrng #-}

idxStrng1 :: VG.Vector v x => Int -> Int -> v x -> IdxStrng v x
idxStrng1 = \minL maxL xs -> IdxStrng (max minL 1) maxL xs
{-# Inline idxStrng1 #-}

instance Build (IdxStrng v x)

instance
  ( Element ls i
  ) => Element (ls :!: IdxStrng v x) i where
  data Elm (ls :!: IdxStrng v x) i = ElmStrng !Int !Int !i !i !(Elm ls i) -- type functions
  type Arg (ls :!: IdxStrng v x)   = Arg ls :. (Int,Int)
  getArg (ElmStrng i j _ _ ls) = getArg ls :. (i, j)
  getIdx (ElmStrng _ _ i _ _ ) = i
  getOmx (ElmStrng _ _ _ o _ ) = o -- outside structuren
  {-# Inline getArg #-}
  {-# Inline getIdx #-}
  {-# Inline getOmx #-}

deriving instance (Show i, Show x, Show (Elm ls i)) => Show (Elm (ls :!: IdxStrng v x) i)

type instance TermArg (TermSymbol a (IdxStrng v x)) = TermArg a :. v x
