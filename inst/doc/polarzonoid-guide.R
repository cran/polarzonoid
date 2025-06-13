## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
old_opt = options( width=144 )

require("rgl",quietly=TRUE)
rgl::setupKnitr(autoprint = TRUE)

## ----echo=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------------
library(polarzonoid)
library(zonohedra,quietly=TRUE)

## ----echo=TRUE,  message=TRUE,  warning=TRUE, fig.width=8, fig.height=4, fig.cap='Figure 3.0 examples of collections of pairwise disjoint arcs', out.width="95%", cache=FALSE----

oldpar = par( mfrow=c(1,2)  , omi=c(0.1,0.1,0.1,0.1), mai=c(0.45,0.5,0.1,0) )
arcmat = matrix( c(1.5,2.9), nrow=1, ncol=2, byrow=TRUE )
plotarcs( arcmat, main=NA )

arcmat = matrix( c(1.5,2, pi,0.5, 5,pi/4 ), nrow=3, ncol=2, byrow=TRUE )
plotarcs( arcmat, main=NA ) ;  par( oldpar )

## ----echo=TRUE,  message=TRUE,  warning=TRUE, fig.align='center', fig.width=7.5, fig.height=5, fig.cap='polar zonohedron with 25 generators &emsp;&emsp; [this is an interactive WebGL widget, try it !]',   fig.keep='none', fig.show='hide', out.width="100%", cache=FALSE----
plot( zonohedra::polarzonohedron( 25, height=2*pi ), type='f', falpha=1  )
rgl::arc3d( 1.005*c(2,0,pi), 1.005*c(0,2,pi), center=c(0,0,pi), base=c(0,1), color='red', lwd=3 )
rgl::rglwidget( webgl=TRUE )

## ----echo=FALSE, results='asis'-----------------------------------------------
options(old_opt)
sessionInfo()

