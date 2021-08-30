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

N: e_HPKN
N: f_MPKN
N: g_LPKN
N: h_KPKN

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
a_Struct -> pkn <<< e_HPKN a_Struct

b_Closed -> hairpin      <<< regionCtx
b_Closed -> interior     <<< regionCtx b_Closed regionCtx
b_Closed -> mlr          <<< nt c_M d_M1 nt

c_M -> mcm_1             <<< regionCtx b_Closed
c_M -> mcm_2             <<< c_M b_Closed
c_M -> mcm_3             <<< c_M nt

d_M1 -> ocm_1            <<< b_Closed
d_M1 -> ocm_2            <<< d_M1 nt

e_HPKN -> hpkn           <<< j_X k_Y j_X k_Y
f_MPKN -> mpkn           <<< j_X l_Z j_X k_Y l_Z k_Y          -- :: A C A B C B     :: 1 3 1 2 3 2     :: X Z X Y Z Y
g_LPKN -> lpkn           <<< j_X k_Y l_Z j_X k_Y l_Z          -- :: A B C A B C     :: 1 2 3 1 2 3     :: X Y Z X Y Z
h_KPKN -> kpkn           <<< j_X k_Y l_Z j_X i_W k_Y l_Z i_W  -- :: A B C A D B C D :: 1 2 3 1 4 2 3 4 :: X Y Z X W Y Z W

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
