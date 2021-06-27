-- |

module BioInf.GenussFold.Grammars.AU where

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
N: <X,2>  -- :: Split-NonTerminal :: A
N: <Y,2>  -- :: Split-NonTerminal :: M
N: <Z,2>  -- :: Split-NonTerminal :: K1
N: <W,2>  -- :: Split-NonTerminal :: K2
T: c      -- :: Terminal
S: S      -- :: Startsymbol

S -> nil <<< e
S -> unp <<< c S
S -> pkn <<< X X

-- ( -  M1 -  )
-- ( K1 M2 K2 )
<X,X> -> pk1  <<< [-,Z] <Y,Y> [-,Z]

-- ( -  -  M1 K1 )  | ( K1 )
-- ( K1 K2 M2 K2 )  | ( K2 )
<Y,Y> -> pk2   <<< [-,Z] [-,W] <Y,Y> [-,W] [Z,-]
<Y,Y> -> pk2_b <<< [-,Z] [Z,-]

-- ( - - K1 [ S )
-- ( ] S K2 - - )
<Z,Z> -> pk3 <<< [-,c] [-,S] <Z,Z> [c,-] [S,-]
<W,W> -> pk4 <<< [-,c] [-,S] <Z,Z> [c,-] [S,-]
//

Emit: PKN
|]

makeAlgebraProduct ''SigPKN
