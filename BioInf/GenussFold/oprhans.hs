-- |

module Git.GenussFoldEnergyMin.BioInf.GenussFold.Orphans where

import           ADP.Fusion.Base
import           ADP.Fusion.Term.Epsilon.Type
import           ADP.Fusion.Base.Subword

instance
  ( Monad m
  , TerminalStream m a is
  ) => TerminalStream m (Term Symbol a Epsilon) (is:.Subword) where
  terminalStream (a:|Epsilon) (sv:.IStatic _) (is:.i@(Subword j))
    = S.map (\(S6 s (zi:._) (zo:._) is os e) -> S6 s zi zo (is:.Subword j) (os:.Subword 0) (e:.()))
    . iPackTerminalStream a sv (is:.i)
  {-# Inline terminalStream #-}
