-- |

module BioInf.GenussFold.Grammars.DP where

import ADP.Fusion
import ADP.Fusion.Base.Subword
import FormalLanguage

{-
Tape1 ::  - - X1 [ S
Tape2 ::  ] S X2 - -
-}

-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: PKN
N: S      -- :: NonTerminal
N: <X,2>  -- :: Split-NonTerminal
N: <Y,2>  -- :: Split-NonTerminal
T: c      -- :: Terminal
S: S      -- :: Startsymbol

S -> nil <<< e
S -> unp <<< c S
S -> jux <<< c S c S
S -> pkn <<< X Y X Y

<X,X> -> pk1  <<< [-,c] [-,S] <X,X> [c,-] [S,-]
<X,X> -> pk1b <<< [c,-] [-,c] [S,-] [-,S]

<Y,Y> -> pk2  <<< [-,c] [-,S] <Y,Y> [c,-] [S,-]
<Y,Y> -> pk2b <<< [c,-] [-,c] [S,-] [-,S]
//
Emit: PKN
|]


makeAlgebraProduct ''SigPKN
