setwd("//media//kswada//MyFiles//R//horse_kicks")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death by horse kick
#  - Data from Ladislaus von Bortkiewicz (1898) on deaths of soldiers in the Prussian army from kicks by horses and mules.
#    He, an economist and statistician, tabulated the number of soldiers in each of 14 army corps in the 20 years from 1875 - 1894
#    who died after being kicked by a horse.
# ------------------------------------------------------------------------------
data("HorseKicks", package = "vcd")

data <- HorseKicks

data
sum(data)



# ------------------------------------------------------------------------------
# Data exploration:  barplot
k = names(data)

par(mfrow=c(1,1))
b <- barplot(data, names.arg = k, xlab = "Number of deaths", ylab = "Frequency", col = "lightblue", cex.lab = 1.5)



