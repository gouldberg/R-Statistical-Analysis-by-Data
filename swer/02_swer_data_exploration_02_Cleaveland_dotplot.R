setwd("//media//kswada//MyFiles//R//swer")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Extreme rainfall in Switzerland
# ------------------------------------------------------------------------------

data(swer, package = "gamair")

str(swer)


head(swer)

car::some(swer)



# ------------------------------------------------------------------------------
# Data exploration:  multi-panel Cleveland dotplot
# ------------------------------------------------------------------------------

library(lattice)


Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)), groups=FALSE,
               strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE), y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data from text file", cex = 1.5))
  
  print(P)  
}


# ----------
graphics.off()

MyVar1 <- c("year", "exra", "nao", "location", "elevation", "climate.region")


Mydotplot(as.matrix(swer[,MyVar1]))



# -->
# Note that some years records are missing for some station



# ------------------------------------------------------------------------------
# More closely for exra and nao
# ------------------------------------------------------------------------------

par(mfrow = c(1,2), mar = c(5,5,2,2))

dotchart(swer$exra, cex.lab = 1.5, xlab = "exra", ylab = "Order of the data")

dotchart(swer$nao, cex.lab = 1.5, xlab = "nao", ylab = "Order of the data")



