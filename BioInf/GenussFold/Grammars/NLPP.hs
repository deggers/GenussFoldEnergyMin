
module BioInf.GenussFold.Grammars.NLPP where

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
N: <W,2>
N: <X,2>
N: <Y,2>
N: <Z,2>
T: c
S: S

T -> nil <<< e
T -> unp <<< c T
T -> jux <<< c T c T

-- Reason :: T X Y X Y cannot be resolved
-- But :: X Y X Y T can be resolved
U -> hPk <<< X Y X Y
U -> kPk <<< X Y X Z Y Z
U -> lPk <<< X Y Z X Y Z
U -> mPk <<< X Y Z X W Y Z W

S -> nil <<< e
S -> unp <<< c T
S -> jux <<< c T c T
S -> tmp <<< T U

<W,W> -> pk4 <<< [-,c] [-,T] <W,W> [c,-] [T,-]
<W,W> -> pk4b <<< [c,-] [-,c] [T,-] [-,T]

<X,X> -> pk1 <<< [-,c] [-,T] <X,X> [c,-] [T,-]
<X,X> -> pk1b <<< [c,-] [-,c] [T,-] [-,T]

<Y,Y> -> pk2 <<< [-,c] [-,T] <Y,Y> [c,-] [T,-]
<Y,Y> -> pk2b <<< [c,-] [-,c] [T,-] [-,T]

<Z,Z> -> pk3 <<< [-,c] [-,T] <Z,Z> [c,-] [T,-]
<Z,Z> -> pk3b <<< [c,-] [-,c] [T,-] [-,T]

//
Emit: PKN
|]


makeAlgebraProduct ''SigPKN
