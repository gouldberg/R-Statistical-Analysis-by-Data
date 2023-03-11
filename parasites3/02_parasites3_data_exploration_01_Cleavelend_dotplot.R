setwd("//media//kswada//MyFiles//R//parasites3")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  parasites3
# ------------------------------------------------------------------------------

Par <- read.table(file = "Parasites3.txt", header = TRUE)


str(Par)

dim(Par)



# ----------
Par$Worms <- Par$Elytrophalloides_oatesi



# ------------------------------------------------------------------------------
# Observations considerable smaller or larger than the majority:  multi-panel Cleveland dotplots
# ------------------------------------------------------------------------------

Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)),
               groups=FALSE,
               strip = strip.custom(bg = 'white',
                                    par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE),
                             y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data", cex = 1.5))
  
  print(P)  
}


MyVar <- c("Length", "Weight")

Mydotplot(Par[, MyVar])


