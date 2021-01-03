-- |

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
  ) => MkStream m (ls :!: Strng v x) Subword where
  mkStream (ls :!: Strng slice mn mx v) (IStatic ()) hh (Subword (i:.j))
    = S.filter (\s -> let Subword (k:.l) = getIdx s in l-k <= mx)
    . S.map (\s -> let (Subword (_:.l)) = getIdx s
                   in  ElmStrng (slice l (j-l) v) (subword l j) (subword 0 0) s)
    $ mkStream ls (IVariable ()) hh (delay_inline Subword (i:.j - mn))
  mkStream (ls :!: Strng slice mn mx v) (IVariable ()) hh (Subword (i:.j))
    = S.flatten mk step Unknown $ mkStream ls (IVariable ()) hh (delay_inline Subword (i:.j - mn))
    where mk s = let Subword (_:.l) = getIdx s in return (s :. j - l - mn)
          step (s:.z) | z >= 0 = do let Subword (_:.k) = getIdx s
                                        l              = j - z
                                        kl             = subword k l
                                    return $ S.Yield (ElmStrng (slice k (l-k) v) kl (subword 0 0) s) (s:.z-1)
                      | otherwise = return $ S.Done
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline mkStream #-}

-- makes this a comment :: ->

instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I) -- @TODO is,Int ?
--  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  , KnownNat minSz
  ) ⇒ TermStream m (ps:.IVariable d) (TermSymbol ts (Str Nothing minSz Nothing v x)) s (is:.Subword I) where
  termStream Proxy (ts:|Str xs) (us:..LtSubword u) (is:.Subword (i:.j))
    = S.flatten mk step . termStream (Proxy ∷ Proxy ps) ts us is
    where mk (tstate@(TState s ii ee)) =
            let RiSwI k = getIndex (getIdx s) (Proxy ∷ PRI is (Subword I)) -- extrahiert laufindex k = index vom nt
                msz     = fromIntegral $ natVal (Proxy ∷ Proxy minSz) -- minimialSize - wie groß soll region sein? da nicht empty mind 1
            in  return (tstate,k+msz)
          step (TState s ii ee, curK)
            | curK > j  = return $ S.Done
            | otherwise =
                let RiSwI k = getIndex (getIdx s) (Proxy ∷ PRI is (Subword I)) -- RiSwI -> Running Inside Subword Iindex
--                in  return $ S.Yield (TState s (ii:.:RiSwI curK) (ee:.VG.unsafeSlice k (curK-k) xs))
                  in  return $ S.Yield (TState s (ii:.:RiSwI curK) (ee:.(k, curK))) -- @TODO k began with -> curK currently at
                                     (TState s ii ee, curK +1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline termStream #-}
