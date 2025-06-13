## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
old_opt = options( width=144 )

## ----echo=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------------
library(polarzonoid)

## ----echo=FALSE, message=TRUE-----------------------------------------------------------------------------------------------------------------
library( flextable )
requireNamespace('equatags',quietly=TRUE)

rhs <- c(
  "\\gamma_n(z_1,...,z_n,L)",
  "z_1",
  "\\cos(L/2)z_1^2 ~-~ 2 \\sin(L/2)z_2",
  "12 ( 4\\sin^2(L/2) ~-~ |z_1|^2 ) z_3 \\newline { ~+~ (8\\cos^2(L/2) - |z_1|^2 + 4) z_1^3 } \\newline ~-~ 48 \\sin(L/2)\\cos(L/2) z_1 z_2 ~+~ 12 \\overline{z_1} z_2^2"
  )

lhs = c(
    "\\rho_n(z_1,...,z_n,L)",
    "2\\sin(L/2)", 
    "4\\sin^2(L/2) ~-~ |z_1|^2", 
    "{ 6\\sin(L/2) (|z_1|^4 - 8|z_1|^2 - 4|z_2|^2 ) }  \\newline { ~+~ 12\\cos(L/2)(\\overline{z_1}^2z_2 + z_1^2\\overline{z_2}) }   \\newline ~+~ 96\\sin^3(L/2)" )

df <- data.frame( n=c('n',1:3), gamma=rhs, rho=lhs ) # ; df

ft <- flextable(df)
ft <- compose( x=ft, j="n", value = as_paragraph(as_equation(n, width=0.5)))
ft <- compose( x=ft, j="gamma", value = as_paragraph(as_equation(gamma, width=4.2)))
ft <- compose( x=ft, j="rho", value = as_paragraph(as_equation(rho, width=4.2)))
ft <- align( ft, align = "center", part = "all")
ft <- width( ft, j=c(2,3), width=4.2 )
ft <- delete_part( ft, part="header" )

#  add borders
thick_border = fp_border_default(color="black", width = 2)
thin_border = fp_border_default(color="gray", width = 1)
ft <- border_inner_h( ft, part="all", border = thin_border )
ft <- border_inner_v( ft, part="all", border = thin_border )
ft <- border_outer( ft, part="all", border = thick_border )
ft <- hline( ft, i=1, border=thick_border )
ft <- vline( ft, i=1, j=c(1,2), border=thick_border )

ft

## ----echo=FALSE, results='asis'-----------------------------------------------
options(old_opt)
sessionInfo()

