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
N: U
N: <X,2>
N: <Y,2>
T: c
S: S

T -> nil <<< e
T -> unp <<< c T
T -> jux <<< c T c T

-- think about a name
-- Reason :: T X Y X Y cannot be resolved
-- But :: X Y X Y T can be resolved
U -> pkn <<< X Y X Y -- N hoch 5 statt vorher n hoch 6 - minimale laufzeit verbesserung

S -> nil <<< e
S -> unp <<< c T
S -> jux <<< c T c T
S -> tmp <<< T U  -- addition vom pk-free mit nem pk-teil :: annahme lineare energy modell

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
