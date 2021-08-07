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
        let energy = round (r * 100) :: Int
        printf "%s   %d\n" l energy
        printf "%s   %d\n" (BS.unpack bs) energy
