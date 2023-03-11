setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ------------------------------------------------------------------------------
# Data exploration:  multi-panel Cleveland dotplot
# ------------------------------------------------------------------------------
Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)), groups=FALSE,
               strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE), y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data from text file", cex = 1.5))
  
  print(P)  
}


MyVar <- c("Lat", "Depth", "ML", "d15N")

Mydotplot(as.matrix(Squid[,MyVar]))



# -->
# There are no observations with extremely low or high values of ML and d15N.
# Many observations have similar values for Depth as well as for Lat, altough they are continuous variables.
# This may be a concern.
# Sampling took place during cruises from 1995 to 2003. We have specimens caught on the same cruise, which may potentially introduce a dependency structure.
# If squid of this species float in groups, our sample may contain multiple observations from a group exposed to
# similar environmental conditions. The d15N ratios of specimens from te same sample may potentially be more similar than d15N ratios of specimens
# from different samples.
