## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
old_opt = options( width=144 )

## ----echo=TRUE, message=FALSE-----------------------------------------------------------------------------------------------------------------
library(polarzonoid)

## ----echo=TRUE, message=FALSE-----------------------------------------------------------------------------------------------------------------

GIFfromarclist <- function( arclist, arcmat, index=1L, fps=5, vpsize=c(480,512) )
    {
    require( 'gifski' )

    # make temp folder
    pathtemp = tempdir()   # "./figs"     ;   if( ! file.exists(pathtemp) ) dir.create(pathtemp)
    count   = length( arclist )
    namevec = names( arclist )

    for( k in 1:count )
        {
        filename    = sprintf( "%s/plot%03d.png", pathtemp, k )
        png( filename=filename, width=vpsize[1], height=vpsize[2], units = "px" )
        u   = spherefromarcs( arclist[[k]] )
        plotarcs( arclist[[k]], labels=FALSE, margintext=namevec[k] )
        plotarcs( arcmat, labels=FALSE, rad=0.95, col='blue', lwd=1, add=TRUE )
        dev.off()
        }

    pathvec = dir( pathtemp, pattern="png$", full=T )
    gif_file = sprintf( "%s/animation%g.gif", pathtemp, index )
    out = gifski( pathvec, gif_file=gif_file, delay=1/fps, progress=F, width=vpsize[1], height=vpsize[2] )
    res = file.remove( pathvec )  # cleanup the .PNG files, leaving just the .GIF

    return(out)
    }

## ----echo=TRUE, message=FALSE-----------------------------------------------------------------------------------------------------------------
circleofarcs <- function( arcmat, rad=0.1, count=180 )
    {
    res = spherefromarcs_plus( arcmat, n=nrow(arcmat)+1L )
    
    out     = vector( count, mode='list' )
    namevec = character( count )
    
    for( i in 1:count )
        {
        theta   = 2*pi * (i-1)/count     # theta is in radians, starting at 0
        u   = res$u  +  rad * ( cos(theta)*res$normal[ ,1]  +  sin(theta)*res$normal[ ,2] )
        out[[i]]      = arcsfromsphere( u )     # u is automatically unitized
        namevec[i]    = sprintf( "i = %d", i )
        }

    names(out)  = namevec

    return( out )
    }

## ----echo=TRUE, message=FALSE, warning=TRUE, fig.cap='caption', fig.keep='last', fig.show='hide', cache=FALSE---------------------------------
# arcmat1 is a single semicircle centered at (1,0)
arcmat1 = matrix( c(0,pi), nrow=1, ncol=2 )
circle  = circleofarcs( arcmat1, count=90 )
gif_file = GIFfromarclist( circle, arcmat1, index=1, vpsize=c(480,480) )

## ----echo=FALSE, message=TRUE, warning=TRUE---------------------------------------------------------------------------------------------------
unlink( dirname(gif_file) )

## ----echo=TRUE, message=FALSE, warning=TRUE, fig.cap='caption', fig.keep='last', fig.show='hide', cache=FALSE---------------------------------
# arcmat2 is: an arc filling quadrant #1, plus an arc filling quadrant #3
arcmat2 = matrix( c((1/4)*pi,pi/2, (5/4)*pi,pi/2), nrow=2, ncol=2, byrow=TRUE )
circle  = circleofarcs( arcmat2, count=90 )
gif_file = GIFfromarclist( circle, arcmat2, index=2, vpsize=c(480,480) )

## ----echo=FALSE, message=TRUE, warning=TRUE---------------------------------------------------------------------------------------------------
unlink( dirname(gif_file) )

## ----echo=TRUE, message=FALSE-----------------------------------------------------------------------------------------------------------------
poletopole <- function( arcmat, thetamax=pi/36, n=NULL )
    {
    u  = spherefromarcs( arcmat, n=n )

    #  make south and north poles
    m       = length(u)    
    south   = c( rep(0,m-1), -1 ) ;      north   = -south
    
    path1   = slerp( south, u, thetamax=thetamax )   #   from "south pole" to u
    path2   = slerp( u, north, thetamax=thetamax )   #   from u to "north pole"

    path    = rbind( path1, path2 ) # concatenate the 2 paths
    path    = rbind( path, -path )  # back down the other side to south pole again
    
    count   = nrow(path)
    out     = vector( count, mode='list' )
    for( i in 1:count )
        out[[i]] = arcsfromsphere( path[i, ] )

    names(out)  = sprintf( "y_%d = %.3f", m, path[ ,m] )

    return( out )    
    }

## ----echo=TRUE, message=FALSE, warning=TRUE, fig.cap='caption', fig.keep='last', fig.show='hide', cache=FALSE---------------------------------
# arcmat3 is 3 arcs of different lengths
arcmat3 = matrix( c(0.375,0.75,  2.3,1.1,  4.6,2.8), ncol=2, byrow=TRUE )
arclist  = poletopole( arcmat3 )
gif_file = GIFfromarclist( arclist, arcmat3, index=3, fps=2, vpsize=c(480,480) )

## ----echo=FALSE, message=TRUE, warning=TRUE---------------------------------------------------------------------------------------------------
unlink( dirname(gif_file) )

## ----echo=TRUE, message=TRUE, warning=TRUE, fig.cap='caption', fig.keep='last', fig.show='hide', cache=FALSE----------------------------------
# arcmat1 is a single arc, but it splits into 3 arcs on either side of the path from pole to pole
arcmat1 = matrix( c(1.5,2.9), ncol=2, byrow=TRUE )
arclist  = poletopole( arcmat1, n=3 )
gif_file = GIFfromarclist( arclist, arcmat1, index=4, fps=2, vpsize=c(480,480) )

## ----echo=FALSE, message=TRUE, warning=TRUE---------------------------------------------------------------------------------------------------
unlink( dirname(gif_file) )

## ----echo=FALSE, results='asis'-----------------------------------------------
options(old_opt)
sessionInfo()

