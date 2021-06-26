-- |

module BioInf.GenussFold.Grammars.ViennaRNA where

import ADP.Fusion
import ADP.Fusion.Base.Subword
import FormalLanguage

-- @TODO  hairpin -> regionCtx 5 ... 32  3 empty 2 paired to ensure only semtical correct parses
-- | Define signature and grammar
[formalLanguage|
Verbose

Grammar: EnergyMin
N: a_Struct
N: b_Closed
N: c_M
N: d_M1

T: nt
T: regionCtx

S: a_Struct

a_Struct -> unpaired     <<< nt a_Struct
a_Struct -> juxtaposed   <<< b_Closed a_Struct
a_Struct -> nil          <<< e

b_Closed -> hairpin      <<< regionCtx
b_Closed -> interior     <<< regionCtx b_Closed regionCtx
b_Closed -> mlr          <<< nt c_M d_M1 nt

c_M -> mcm_1             <<< regionCtx b_Closed
c_M -> mcm_2             <<< c_M b_Closed
c_M -> mcm_3             <<< c_M nt

d_M1 -> ocm_1            <<< b_Closed
d_M1 -> ocm_2            <<< d_M1 nt

//
Emit: EnergyMin
|]

makeAlgebraProduct ''SigEnergyMin
