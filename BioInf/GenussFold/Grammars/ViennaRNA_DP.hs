-- |

module BioInf.GenussFold.Grammars.ViennaRNA_DP where

import ADP.Fusion
import ADP.Fusion.Base.Subword
import FormalLanguage

-- | Define signature and grammar
-- Compared to R&G nesting is moved from initial closure to final closure yielding

[formalLanguage|
Verbose

Grammar: EnergyMin
N: S
N: T
N: a_Struct
N: b_Closed
N: c_M
N: d_M1
N: <f_X,2>
N: <g_Y,2>

T: nt
T: regionCtx

S: a_Struct

a_Struct -> nil <<< e
a_Struct -> unp <<< nt a_Struct
a_Struct -> jux <<< b_Closed a_Struct
a_Struct -> hpk <<< f_X g_Y f_X g_Y

b_Closed -> hairpin      <<< regionCtx
b_Closed -> interior     <<< regionCtx b_Closed regionCtx
b_Closed -> mlr          <<< nt c_M d_M1 nt

c_M -> mcm_1             <<< regionCtx b_Closed
c_M -> mcm_2             <<< c_M b_Closed
c_M -> mcm_3             <<< c_M nt

d_M1 -> ocm_1            <<< b_Closed
d_M1 -> ocm_2            <<< d_M1 nt

<f_X,f_X> -> pk1 <<< [-,regionCtx] [-,a_Struct] <f_X,f_X> [regionCtx,-] [a_Struct,-]
<f_X,f_X> -> pk1b <<< [nt,-] [-,nt] [a_Struct,-] [-,a_Struct]

<g_Y,g_Y> -> pk2 <<< [-,regionCtx] [-,a_Struct] <g_Y,g_Y> [regionCtx,-] [a_Struct,-]
<g_Y,g_Y> -> pk2b <<< [nt,-] [-,nt] [a_Struct,-] [-,a_Struct]

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin
