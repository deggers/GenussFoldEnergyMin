-- |

module BioInf.GenussFold.Grammars.ViennaRNA_LP where

import ADP.Fusion
import ADP.Fusion.Base.Subword
import FormalLanguage

[formalLanguage|
Verbose

Grammar: EnergyMin
N: S
N: T
N: a_Struct
N: b_Closed
N: c_M
N: d_M1
N: e_PKN
N: <X,2>
N: <Y,2>

T: nt
T: regionCtx

S: a_Struct

a_Struct -> unpaired     <<< nt a_Struct
a_Struct -> juxtaposed   <<< b_Closed a_Struct
a_Struct -> pkn          <<< b_Closed e_PKN
a_Struct -> nil          <<< e

b_Closed -> hairpin      <<< regionCtx
b_Closed -> interior     <<< regionCtx b_Closed regionCtx
b_Closed -> mlr          <<< nt c_M d_M1 nt

c_M -> mcm_1             <<< regionCtx b_Closed
c_M -> mcm_2             <<< c_M b_Closed
c_M -> mcm_3             <<< c_M nt

d_M1 -> ocm_1            <<< b_Closed
d_M1 -> ocm_2            <<< d_M1 nt

e_PKN -> hpk             <<< X Y X Y

<X,X> -> pk1 <<< [-,nt] [-,b_Closed] <X,X> [nt,-] [b_Closed,-]
<X,X> -> pk1b <<< [nt,-] [-,nt] [b_Closed,-] [-,b_Closed]

<Y,Y> -> pk2 <<< [-,nt] [-,b_Closed] <Y,Y> [nt,-] [b_Closed,-]
<Y,Y> -> pk2b <<< [nt,-] [-,nt] [b_Closed,-] [-,b_Closed]

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin
