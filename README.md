[![Build Status](https://travis-ci.org/choener/GenussFold.svg?branch=master)](https://travis-ci.org/choener/GenussFold)

# Framework: GenussFold: RNA Pseudoknot Folding

[*generalized Algebraic Dynamic Programming Homepage*](http://www.bioinf.uni-leipzig.de/Software/gADP/)

The implementation makes use of the *gADP* technique and provides a larger
example on how to implement algorithms that require interleaved, split
syntactic variables.

# Thesis : Evaluation of pseudoknot-including RNA structure prediction grammars


# How to run 
Just run "ghci stack" - the nix-shell cannot work right now as it takes the newest dependencies which are not working right now 

❯ echo "AAACCCUUUAAAGGGUUU" | stack run -- -p 1 -c 10    


#### Credit

Christian Hoener zu Siederdissen  
Leipzig University, Leipzig, Germany  
choener@bioinf.uni-leipzig.de  
http://www.bioinf.uni-leipzig.de/~choener/  

Formal background can be found in this paper:

Maik Riechert, Christian Höner zu Siederdissen, and Peter F. Stadler  
*Algebraic dynamic programming for multiple context-free languages*  
2015, submitted  
[preprint](http://www.bioinf.uni-leipzig.de/Software/gADP/preprints/rie-hoe-2015.pdf)  
