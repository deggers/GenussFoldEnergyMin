-- |

module BioInf.GenussFold.Grammars.KHP_Nussinov where

import ADP.Fusion
import ADP.Fusion.Base.Subword
import FormalLanguage

-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: PKN
N: S
N: <X,2>
N: <Y,2>
N: <Z,2>
T: c
S: S

S -> unp <<< c S
S -> nil <<< e
S -> jux <<< c S c S
S -> khp <<< X Y X Z Y Z S

-- Extract a common NP :: <X,X> -> <C,C>  needs recursiveTables
-- LEFT-HAIRPIN
<X,X> -> pk1 <<< [c,-] <X,X> [-,c]
<X,X> -> nll <<< [e,e]

-- CONNECTING-HELICE
<Y,Y> -> pk2 <<< [c,-] <Y,Y> [-,c]
<Y,Y> -> nll <<< [e,e]

-- RIGHT-HAIRPIN
<Z,Z> -> pk3 <<< [c,-] <Z,Z> [-,c]
<Z,Z> -> nll <<< [e,e]

//
Emit: PKN
|]

makeAlgebraProduct ''SigPKN
