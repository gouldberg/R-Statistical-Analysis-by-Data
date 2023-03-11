setwd("//media//kswada//MyFiles//R//depends")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Depends
#  - Frequency distribution of the number of dependencies declared in 4,983 R packages
#    maintained on the CRAN distribution network on January 17, 2014.
# ------------------------------------------------------------------------------
data("Depends", package = "vcdExtra")

data <- Depends

data



# ------------------------------------------------------------------------------
# barplot
# ------------------------------------------------------------------------------
k = names(data)

par(mfrow=c(1,1))
b <- barplot(data, names.arg = k, xlab = "Number of dependencies", ylab = "Frequency")
lines(x = b, y = data, col = "red")



