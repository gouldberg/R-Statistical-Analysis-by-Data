# setwd("//media//kswada//MyFiles//R//teeth_nitrogen")
setwd("//media//kswada//MyFiles//R//Variance_and_correlation_structure_model//teeth_nitrogen")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TeethNitrogen
# ------------------------------------------------------------------------------


TN <- read.table(file = "TeethNitrogen.txt", header = TRUE)


str(TN)


dim(TN)


car::some(TN)



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


# ----------
graphics.off()

MyVar1 <- names(TN)


# Shown in scaled values
Mydotplot(TN[,MyVar1])





