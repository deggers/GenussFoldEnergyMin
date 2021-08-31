-- |

module BioInf.GenussFold.Grammars.ViennaRNA_DP where

import ADP.Fusion
import ADP.Fusion.Base.Subword
import FormalLanguage

-- | Define signature and grammar

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
N: <f_X,2>
N: <g_Y,2>
N: <h_Z1,2>
N: <h_Z2,2>

T: nt
T: regionCtx

S: a_Struct

a_Struct -> nil <<< e
a_Struct -> unp <<< nt a_Struct
a_Struct -> jux <<< b_Closed a_Struct
a_Struct -> pkn <<< e_PKN a_Struct

b_Closed -> hairpin      <<< regionCtx
b_Closed -> interior     <<< regionCtx b_Closed regionCtx
b_Closed -> mlr          <<< nt c_M d_M1 nt

c_M -> mcm_1             <<< regionCtx b_Closed
c_M -> mcm_2             <<< c_M b_Closed
c_M -> mcm_3             <<< c_M nt

d_M1 -> ocm_1            <<< b_Closed
d_M1 -> ocm_2            <<< d_M1 nt

e_PKN -> hpk             <<< f_X f_X

-- A
<f_X,f_X> -> pk1a <<< [-,h_Z1] <g_Y,g_Y> [-,h_Z1]
<f_X,f_X> -> pk1b <<< [nt,-] [-,nt] [a_Struct,-] [-,a_Struct]

-- M
<g_Y,g_Y> -> pk2a <<< [-,h_Z1] [-,h_Z2] <g_Y,g_Y> [h_Z1,-] [-,h_Z2]
<g_Y,g_Y> -> pk2b <<< h_Z1 h_Z1

-- K :: K1 and K2
<h_Z1,h_Z1> -> pk3a <<< [-,regionCtx] [-,a_Struct] <h_Z1,h_Z1> [regionCtx,-] [a_Struct,-]
<h_Z1,h_Z1> -> pk3b <<< [nt,-] [-,nt] [a_Struct,-] [-,a_Struct]

<h_Z2,h_Z2> -> pk4a <<< [-,regionCtx] [-,a_Struct] <h_Z2,h_Z2> [regionCtx,-] [a_Struct,-]
<h_Z2,h_Z2> -> pk4b <<< [nt,-] [-,nt] [a_Struct,-] [-,a_Struct]

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin
