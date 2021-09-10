-- |

module BioInf.GenussFold.Grammars.ViennaRNA_RN where

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

N: <i_W,2> -- :: D
N: <j_X,2> -- :: A
N: <k_Y,2> -- :: B
N: <l_Z,2> -- :: C

T: nt
T: regionCtx

S: a_Struct

a_Struct -> nil <<< e
a_Struct -> unp <<< nt a_Struct
a_Struct -> jux <<< b_Closed a_Struct

a_Struct -> hpkn <<< j_X k_Y j_X k_Y
a_Struct -> mpkn <<< j_X l_Z j_X k_Y l_Z k_Y
a_Struct -> lpkn <<< j_X k_Y l_Z j_X k_Y l_Z
a_Struct -> kpkn <<< j_X k_Y l_Z j_X i_W k_Y l_Z i_W

b_Closed -> hairpin      <<< regionCtx
b_Closed -> interior     <<< regionCtx b_Closed regionCtx
b_Closed -> mlr          <<< nt c_M d_M1 nt

c_M -> mcm_1             <<< regionCtx b_Closed
c_M -> mcm_2             <<< c_M b_Closed
c_M -> mcm_3             <<< c_M nt

d_M1 -> ocm_1            <<< b_Closed
d_M1 -> ocm_2            <<< d_M1 nt

<i_W,i_W> -> pk1a <<< [-,regionCtx] [-,a_Struct] <i_W,i_W> [regionCtx,-] [a_Struct,-]
<i_W,i_W> -> pk1b <<< [nt,-] [-,nt] [a_Struct,-] [-,a_Struct]

<j_X,j_X> -> pk2a <<< [-,regionCtx] [-,a_Struct] <j_X,j_X> [regionCtx,-] [a_Struct,-]
<j_X,j_X> -> pk2b <<< [nt,-] [-,nt] [a_Struct,-] [-,a_Struct]

<k_Y,k_Y> -> pk3a <<< [-,regionCtx] [-,a_Struct] <k_Y,k_Y> [regionCtx,-] [a_Struct,-]
<k_Y,k_Y> -> pk3b <<< [nt,-] [-,nt] [a_Struct,-] [-,a_Struct]

<l_Z,l_Z> -> pk4a <<< [-,regionCtx] [-,a_Struct] <l_Z,l_Z> [regionCtx,-] [a_Struct,-]
<l_Z,l_Z> -> pk4b <<< [nt,-] [-,nt] [a_Struct,-] [-,a_Struct]

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin
