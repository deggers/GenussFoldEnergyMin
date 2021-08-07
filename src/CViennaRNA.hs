{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Text.Printf
import Control.Monad (forM_)

import qualified Data.ByteString.Char8 as BS
import qualified BioInf.ViennaRNA.Bindings.Inline as V

main :: IO ()
main = do
      ls <- lines <$> getContents
      forM_ ls $ \l -> do
        let (r :: Double,bs) = V.mfe (BS.pack l)
        printf "%s   %d\n" l (round (r * 100) :: Int)
--        forM_ bs $ \[b] -> printf "%s   %d\n" b r
