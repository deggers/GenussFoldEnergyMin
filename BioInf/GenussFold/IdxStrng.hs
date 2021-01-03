-- |
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

data IdxStrng v x where
  Strng :: VG.Vector v x
        => (Int -> Int -> v x -> v x)  -- @slice@ function
        -> Int                         -- minimal size
        -> Int                         -- maximal size (just use s.th. big if you don't want a limit)
        -> (v x)                       -- the actual vector
        -> IdxStrng v x

idxStrng :: VG.Vector v x => Int -> Int -> v x -> IdxStrng v x
idxStrng = \minL maxL xs -> Strng VG.unsafeSlice minL maxL xs
{-# Inline idxStrng #-}

instance Build (IdxStrng v x)

instance
  ( Element ls i
  ) => Element (ls :!: IdxStrng v x) i where
  data Elm (ls :!: IdxStrng v x) i = ElmStrng !(v x) !i !i !(Elm ls i)
  type Arg (ls :!: IdxStrng v x)   = Arg ls :. v x
  getArg (ElmStrng x _ _ ls) = getArg ls :. x
  getIdx (ElmStrng _ i _ _ ) = i
  getOmx (ElmStrng _ _ o _ ) = o
  {-# Inline getArg #-}
  {-# Inline getIdx #-}
  {-# Inline getOmx #-}

deriving instance (Show i, Show (v x), Show (Elm ls i)) => Show (Elm (ls :!: IdxStrng v x) i)

type instance TermArg (TermSymbol a (IdxStrng v x)) = TermArg a :. v x
