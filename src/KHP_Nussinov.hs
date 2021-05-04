
module Main where

import Options.Applicative
import Text.Printf
import Control.Monad (forM_)

import BioInf.GenussFold.KHP_Nussinov

data Options = Options
  { coopts  :: Int
  , penalty :: Int
  }

sample :: Parser Options
sample = Options
      <$> option auto
          ( long "coopts"
         <> short 'c'
         <> help "How many cooptimal structures"
         <> showDefault
         <> value 1
         <> metavar "INT" )
      <*> option auto
         ( long "penalty"
         <> short 'p'
         <> help "the penalty for opening"
         <> showDefault
         <> value 1
         <> metavar "INT" )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "KHP and Nussinov"
     <> header "GenussFold - With Kissing Hairpins and Nussinov" )

greet :: Options -> IO ()
greet (Options c p) = do
      ls <- lines <$> getContents
      forM_ ls $ \l -> do
        let (r,bs) = energyMin c p l
        printf "%s   %d\n" l r
        forM_ bs $ \[b] -> printf "%s   %d\n" b r
