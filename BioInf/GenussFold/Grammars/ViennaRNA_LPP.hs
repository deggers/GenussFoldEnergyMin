-- |

module BioInf.GenussFold.Grammars.ViennaRNA_LPP where

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
N: b_Struct
N: c_Closed
N: d_M
N: e_M1

N: f_HPKN
N: g_KPKN
N: h_MPKN
N: i_LPKN

N: <j_A,2>
N: <k_B,2>
N: <l_C,2>
N: <m_D,2>

T: nt
T: regionCtx

S: a_Struct

a_Struct -> nil <<< e
a_Struct -> unp <<< nt a_Struct
a_Struct -> jux <<< c_Closed a_Struct

a_Struct -> h_pkn <<< b_Struct f_HPKN
a_Struct -> k_pkn <<< b_Struct g_KPKN
a_Struct -> m_pkn <<< b_Struct h_MPKN
a_Struct -> l_pkn <<< b_Struct i_LPKN

f_HPKN   -> hpkn <<< j_A k_B j_A k_B
g_KPKN   -> kpkn <<< j_A l_C j_A k_B l_C k_B
h_MPKN   -> mpkn <<< j_A k_B l_C j_A k_B l_C
i_LPKN   -> lpkn <<< j_A k_B l_C j_A m_D k_B l_C m_D  --  A B C A D B C D

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

<j_A,j_A> -> pk1a <<< [-,regionCtx] [-,b_Struct] <j_A,j_A> [regionCtx,-] [b_Struct,-]
<j_A,j_A> -> pk1b <<< [nt,-] [-,nt] [b_Struct,-] [-,b_Struct]

<k_B,k_B> -> pk2a <<< [-,regionCtx] [-,b_Struct] <k_B,k_B> [regionCtx,-] [b_Struct,-]
<k_B,k_B> -> pk2b <<< [nt,-] [-,nt] [b_Struct,-] [-,b_Struct]

<l_C,l_C> -> pk3a <<< [-,regionCtx] [-,b_Struct] <l_C,l_C> [regionCtx,-] [b_Struct,-]
<l_C,l_C> -> pk3b <<< [nt,-] [-,nt] [b_Struct,-] [-,b_Struct]

<m_D,m_D> -> pk4a <<< [-,regionCtx] [-,b_Struct] <m_D,m_D> [regionCtx,-] [b_Struct,-]
<m_D,m_D> -> pk4b <<< [nt,-] [-,nt] [b_Struct,-] [-,b_Struct]

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin
