## Unreduced BIBD #####
choose(7, 3) ## binomial coefficient
combn(7, 3)  ## all possible combinations

## BIBD ####
library(crossdes)

## Find a BIBD with
## 7 treatments
## 14 blocks
## 3 treatments per block

m <- find.BIB(7, 14, 3)
m
?find.BIB ## see note in help page!
isGYD(m)  ## need to do this check

## alternative: package ibd

library(ibd)
out <- ibd(7, 14, 3)
out$design
out$conc.mat

## Youden Squares ####
m2 <- rbind(c(1, 3, 7, 5, 4, 2, 6),
           c(2, 1, 4, 3, 6, 7, 5),
           c(5, 6, 1, 4, 2, 3, 7))
isGYD(m2)

