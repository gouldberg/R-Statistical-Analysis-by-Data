setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ------------------------------------------------------------------------------
# Observations considerable smaller or larger than the majority:  multi-panel Cleveland dotplots
# ------------------------------------------------------------------------------

library(lattice)

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


MyVar <- c("race", "fire", "theft", "age", "involact", "income")

Mydotplot(chredlin[, MyVar])




# ----------
summary(chredlin)

sum(chredlin$involact == 0)



# -->
# The response involact has a large number of zeros.
# we can see some outlier and influential points.

# Also we notice that there are some relevance among "race" and "involact"




