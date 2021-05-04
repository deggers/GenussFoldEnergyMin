
module Main where
import Options.Applicative
import Control.Applicative ( (<$>) )
import Control.Monad (forM_)
import Text.Printf

-- import BioInf.GenussFold
import BioInf.GenussFold.PKN
import BioInf.GenussFold.ViennaRNA

-- sum type
data Options = Nussinov
  { coopts      :: Int
  }
  | KHP
  {
    coopts :: Int
  , openPenalty :: Int
  }
  deriving (Show)

pNussinov :: Parser Options
pNussinov = Nussinov
      <$> option auto
          ( long "coopts"
         <> short 'c'
         <> help "How many cooptimal structures"
         <> showDefault
         <> value 1
         <> metavar "INT" )

pKHP :: Parser Options
pKHP = KHP
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

pOptions :: Parser Options
pOptions = pNussinov <|> pKHP

main :: IO ()
main = run =<< execParser
  (info pOptions fullDesc)
--  (parseOptions `withInfo` "Fold RNA with Pseudoknots")

-- actual program logic
run :: Options -> IO ()
run (cmd) = do
  case cmd of
    Nussinov{..} -> do
      ls <- lines <$> getContents
      forM_ ls $ \l -> do
        let (r,bs) = energyMin coopts l
        printf "%s   %f\n" l r
        forM_ bs $ \[b] -> printf "%s   %f\n" b r
    KHP c p -> do
      ls <- lines <$> getContents
      forM_ ls $ \l -> do
        let (r,bs) = pknPairMax c p l
        printf "%s   %d\n" l r
        forM_ bs $ \[b] -> printf "%s   %d\n" b r

{--
main = do
  o <- cmdArgs $ modes [oNussinov]
  case o of
    Nussinov{..} -> do
      ls <- lines <$> getContents
      forM_ ls $ \l -> do
        let (r,bs) = energyMin coopts l
        printf "%s   %d\n" l r
        forM_ bs $ \[b] -> printf "%s   %d\n" b r
    KHP{..} -> do
      ls <- lines <$> getContents
      forM_ ls $ \l -> do
        let (r,bs) = energyMin coopts l
        printf "%s   %d\n" l r
        forM_ bs $ \[b] -> printf "%s   %d\n" b r
--}
