-- |

module BioInf.GenussFold.Grammars.RG where

import ADP.Fusion
import ADP.Fusion.Base.Subword
import FormalLanguage

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

-- TODO do I need for the only-closing case?
<X,X> -> pk1 <<< [-,c] [-,S] <X,X> [c,-] [S,-]
<X,X> -> nll <<< [e,e]

<Y,Y> -> pk2 <<< [-,c] [-,S] <Y,Y> [c,-] [S,-]
<Y,Y> -> nll <<< [e,e]
//
Emit: PKN
|]


makeAlgebraProduct ''SigPKN
