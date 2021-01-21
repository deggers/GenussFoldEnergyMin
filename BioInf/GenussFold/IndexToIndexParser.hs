-- |
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module BioInf.GenussFold.IndexToIndexParser where

-- http://hackage.haskell.org/package/ADPfusion-0.4.1.1/docs/src/ADP-Fusion-Term-Strng-Type.html :: use as example for type like XStrng
-- http://hackage.haskell.org/package/ADPfusion-0.4.1.1/docs/src/ADP-Fusion-Term-Strng-Subword.html :: use this as template

import           Data.Strict.Tuple
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Fusion.Util (delay_inline)
import           Debug.Trace
import           Prelude hiding (map)
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic as VG

import           Data.PrimitiveArray

import           ADP.Fusion.Base
import           BioInf.GenussFold.IdxStrng

instance
  ( Monad m
  , Element ls Subword
  , MkStream m ls Subword
  ) => MkStream m (ls :!: IdxStrng v x) Subword where
  mkStream (ls :!: IdxStrng mn mx v) (IStatic ()) hh (Subword (i:.j))
    = S.filter (\s -> let Subword (k:.l) = getIdx s in l-k <= mx)
    . S.map (\s -> let (Subword (_:.l)) = getIdx s
                   in  ElmStrng l j (subword l j) (subword 0 0) s)
    $ mkStream ls (IVariable ()) hh (delay_inline Subword (i:.j - mn))
  mkStream (ls :!: IdxStrng mn mx v) (IVariable ()) hh (Subword (i:.j))
    = S.flatten mk step Unknown $ mkStream ls (IVariable ()) hh (delay_inline Subword (i:.j - mn))
    where mk s = let Subword (_:.l) = getIdx s in return (s :. j - l - mn)
          step (s:.z) | z >= 0 = do let Subword (_:.k) = getIdx s
                                        l              = j - z
                                        kl             = subword k l -- subword auf dem der parser gerade ist
                                    return $ S.Yield (ElmStrng k l kl (subword 0 0) s) (s:.z-1)
                      | otherwise = return $ S.Done
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline mkStream #-}
