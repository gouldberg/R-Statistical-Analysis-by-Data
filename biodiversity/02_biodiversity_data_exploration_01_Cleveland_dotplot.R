setwd("//media//kswada//MyFiles//R//biodiversity")

packages <- c("dplyr", "lattice", "nlme")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Data:  Biodiversity
# ------------------------------------------------------------------------------

Biodiv <- read.table(file = "Biodiversity.txt", header = TRUE)


str(Biodiv)



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


MyVar <- c("Concentration", "MU", "Mesocosm", "Abundance", "Biomass", "Treatment", "Nutrient")

Mydotplot(as.matrix(Biodiv[,MyVar]))

