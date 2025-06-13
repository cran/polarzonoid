## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# old_opt = options( width=144 )

## ----echo=TRUE----------------------------------------------------------------
library( polarzonoid )

## ----echo=TRUE----------------------------------------------------------------
idx = c(1:6,9)

#  make a random unit vector in S^6
set.seed(0)
u = rnorm(7) ;  u = u / sqrt( sum(u^2) )
# embed into S^8
up = spherefromarcs( arcsfromsphere(u), n=4 )
beta = up[idx] / u ; beta
range( diff(beta) )

## ----echo=TRUE----------------------------------------------------------------
count = 50
umat = array( rnorm(count*7), dim=c(count,7) )
umat = umat / sqrt( rowSums(umat^2) )
upmat = t( apply( umat, 1, function(u) { spherefromarcs( arcsfromsphere(u), n=4 ) } ) )
betamat = upmat[ ,idx] / umat
delta = apply( betamat, 1, diff )
range( delta )

## ----echo=FALSE, results='asis'-----------------------------------------------
# options(old_opt)
sessionInfo()

