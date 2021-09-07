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
N: b_Struct
N: c_Closed
N: d_M
N: e_M1
N: f_PKN
N: <g_X,2>
N: <h_Y,2>

T: nt
T: regionCtx

S: a_Struct

a_Struct -> nil <<< e
a_Struct -> unp <<< nt a_Struct
a_Struct -> jux <<< c_Closed a_Struct
a_Struct -> pkn <<< f_PKN a_Struct

b_Struct -> nil <<< e
b_Struct -> unp <<< nt b_Struct
b_Struct -> jux <<< c_Closed b_Struct

c_Closed -> hairpin      <<< regionCtx
c_Closed -> interior     <<< regionCtx c_Closed regionCtx
c_Closed -> mlr          <<< nt d_M e_M1 nt

d_M -> mcm_1             <<< regionCtx c_Closed
d_M -> mcm_2             <<< d_M c_Closed
d_M -> mcm_3             <<< d_M nt

e_M1 -> ocm_1            <<< c_Closed
e_M1 -> ocm_2            <<< e_M1 nt

f_PKN -> hpk             <<< g_X h_Y g_X h_Y

<g_X,g_X> -> pk1 <<< [-,regionCtx] [-,b_Struct] <g_X,g_X> [regionCtx,-] [b_Struct,-]
<g_X,g_X> -> pk1b <<< [nt,-] [-,nt] [b_Struct,-] [-,b_Struct]

<h_Y,h_Y> -> pk2 <<< [-,regionCtx] [-,b_Struct] <h_Y,h_Y> [regionCtx,-] [b_Struct,-]
<h_Y,h_Y> -> pk2b <<< [nt,-] [-,nt] [b_Struct,-] [-,b_Struct]

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin
