-- |

module BioInf.GenussFold.Grammars.NLP where

import ADP.Fusion
import ADP.Fusion.Base.Subword
import FormalLanguage

-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: PKN
N: S
N: T
N: <X,2>
N: <Y,2>
T: c
S: S

T -> nil <<< e
T -> unp <<< c T
T -> jux <<< c T c T

S -> nil <<< e
S -> unp <<< c T
S -> jux <<< c T c T
S -> pkn <<< T X Y X Y

-- - - K1 c T
-- c T K2 - -

<X,X> -> pk1 <<< [-,c] [-,T] <X,X> [c,-] [T,-]
<X,X> -> nll <<< [e,e]

<Y,Y> -> pk2 <<< [-,c] [-,T] <Y,Y> [c,-] [T,-]
<Y,Y> -> nll <<< [e,e]
//
Emit: PKN
|]


makeAlgebraProduct ''SigPKN
