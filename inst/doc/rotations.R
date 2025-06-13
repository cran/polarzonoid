## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
old_opt = options( width=144 )

## ----echo=TRUE, message=FALSE-----------------------------------------------------------------------------------------------------------------
library(polarzonoid)

## ----echo=TRUE, message=FALSE-----------------------------------------------------------------------------------------------------------------

GIFfromrotationlist <- function( rotationlist, index=1L, fps=4, vpsize=c(480,512) )
    {
    require( 'gifski' )

    # make temp folder
    pathtemp = tempdir()   #  "./figs" ;   if( ! file.exists(pathtemp) ) dir.create(pathtemp)

    count   = length( rotationlist )
    
    namevec = names( rotationlist )

    for( k in 1:count )
        {
        filename    = sprintf( "%s/plot%03d.png", pathtemp, k )
        png( filename=filename, width=vpsize[1], height=vpsize[2], units = "px" )
        rotation3x3 = rotationlist[[k]] 
        arcmat = arcsfromrotation( rotation3x3, tol=1.e-7 )
        
        plotarcs( arcmat, labels=FALSE, main=namevec[k], lwd=NA, cex=3, col='blue' )

        rect( -0.83, -0.4, 0.83, 0.4, col='white', border='black' )
        matlabel  = sprintf( "%+.4f", rotation3x3 )
        matlabel  = gsub( '[+]', ' ', matlabel )
        dim(matlabel) = c(3,3)
        matlabel  = capture.output( print(matlabel,quote=FALSE) )
        matlabel  = matlabel[-1]        # ignore the column labels
        matlabel  = substr( matlabel, 5, 1000 )   # ignore the row labels
        text( 0, c(0.3,0,-0.3), matlabel, adj=c(0.51,0.5), cex=1.5, family='mono' )
        dev.off()
        }

    pathvec = dir( pathtemp, pattern="png$", full=T )
    gif_file = sprintf( "%s/animation%g.gif", pathtemp, index )
    out = gifski( pathvec, gif_file=gif_file, delay=1/fps, progress=F, width=vpsize[1], height=vpsize[2] )
    res = file.remove( pathvec )  # cleanup the .PNG files, leaving just the .GIF

    return(out)
    }

## ----echo=TRUE, message=FALSE-----------------------------------------------------------------------------------------------------------------
circleofrotations <- function( axis, count=72+1 )
    {
    out     = vector( count, mode='list' )
    namevec = character( count )
    
    for( i in 1:count )
        {
        theta_deg     = 360 * (i-1)/(count-1) 
        theta         = pi/180 * theta_deg     # theta is in radians, starting at 0
        out[[i]]      = rotationaroundaxis( axis, theta )
        namevec[i]    = sprintf( "rotation around axis [%g,%g,%g]\ntheta=%g deg",
                          axis[1], axis[2], axis[3], theta_deg )
        }

    names(out)  = namevec

    return( out )
    }

## ----echo=TRUE, message=FALSE, warning=TRUE, fig.cap='caption', fig.keep='last', fig.show='hide', cache=FALSE---------------------------------
circle  = circleofrotations( c(0,0,1), count=72+1 )
gif_file = GIFfromrotationlist( circle, index=1, vpsize=c(480,480) )

## ----echo=FALSE, message=TRUE, warning=TRUE---------------------------------------------------------------------------------------------------
unlink( dirname(gif_file) )

## ----echo=TRUE, message=FALSE, warning=TRUE, fig.cap='caption', fig.keep='last', fig.show='hide', cache=FALSE---------------------------------
circle  = circleofrotations( c(0,1,0), count=72+1 )
gif_file = GIFfromrotationlist( circle, index=2, vpsize=c(480,480) )

## ----echo=FALSE, message=TRUE, warning=TRUE---------------------------------------------------------------------------------------------------
unlink( dirname(gif_file) )

## ----echo=TRUE, message=FALSE, warning=TRUE, fig.cap='caption', fig.keep='last', fig.show='hide', cache=FALSE---------------------------------
circle  = circleofrotations( c(1,0,0), count=72+1 )
gif_file = GIFfromrotationlist( circle, index=3, vpsize=c(480,480) )

## ----echo=FALSE, message=TRUE, warning=TRUE---------------------------------------------------------------------------------------------------
unlink( dirname(gif_file) )

## ----echo=TRUE, message=FALSE, warning=TRUE, fig.cap='caption', fig.keep='last', fig.show='hide', cache=FALSE---------------------------------
set.seed(0)
axis    = rnorm(3)
axis    = axis / sqrt( sum(axis^2) )
circle  = circleofrotations( axis, count=72+1 )
gif_file = GIFfromrotationlist( circle, index=4, vpsize=c(480,480) )

## ----echo=FALSE, message=TRUE, warning=TRUE---------------------------------------------------------------------------------------------------
unlink( dirname(gif_file) )

## ----echo=FALSE, results='asis'-----------------------------------------------
options(old_opt)
sessionInfo()

