module Main where

import Options.Applicative
import Text.Printf
import Control.Monad (forM_)

import BioInf.GenussFold.ViennaRNA

data Options = Options
  { coopts  :: NumBT
  , penalty :: PenPK
  }

params :: Parser Options
params = Options
      <$> (NumBT <$> option auto
          ( long "coopts"
         <> short 'c'
         <> help "How many cooptimal structures"
         <> showDefault
         <> value 1
         <> metavar "INT" ))
      <*> option auto
         ( long "penalty"
         <> short 'p'
         <> help "Not applied"
         <> showDefault
         <> value 0
         <> metavar "INT" )

main :: IO ()
main = fold =<< execParser opts
  where
    opts = info (params <**> helper)
      ( fullDesc
     <> progDesc "ViennaRNA"
     <> header "ViennaRNA" )

fold :: Options -> IO ()
fold (Options c p) = do
      ls <- lines <$> getContents
      forM_ ls $ \l -> do
        let (r,bs) = energyMin c p l
        printf "%s   %d\n" l r
        forM_ bs $ \[b] -> printf "%s   %d\n" b r
