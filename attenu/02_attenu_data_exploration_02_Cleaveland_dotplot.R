setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)



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

MyVar1 <- c("accel", "dist", "mag", "event", "station")

Mydotplot(as.matrix(attenu[,MyVar1]))



# ----------
table(attenu$event, useNA = "always")



