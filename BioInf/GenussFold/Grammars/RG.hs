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
S -> pkn <<< X Y X Y   -- X S ::  Y S ::  X S :: Y S

<X,X> -> pk1 <<< [-,c] <X,X> [c,-] [-,S] [S,-]
<X,X> -> pk1b <<< [-,c] [c,-] [-,S] [S,-]

<Y,Y> -> pk2 <<< [-,c] <Y,Y> [c,-] [-,S] [S,-]
<Y,Y> -> pk2b <<< [-,c] [c,-] [-,S] [S,-]
//
Emit: PKN
|]


makeAlgebraProduct ''SigPKN
