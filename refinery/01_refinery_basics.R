setwd("//media//kswada//MyFiles//R//refinery")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  refinery
#   - Data collected at an oil refinery in Texas.
# ------------------------------------------------------------------------------
data(refinery, package = "fda")

str(refinery)


head(refinery)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

op = par(mfrow=c(2,1), mar=c(4, 4, 1, 2)+.1)

plot(Tray47~Time, refinery, pch='*', xlab='', ylab='Tray 47 level')

plot(Reflux~Time, refinery, pch='*', xlab='Time (min)', ylab='Reflux flow')

par(op)



# -->
# top panel: 193 measurements of the amount of petroleum product at tray level 47 in a distillation column in an oil refinery
# bottom panel:  the flow of a vapor into that tray during an experiment.

# The amount of a petroleum product reacts to the change in the flow of a vapor into the tray.
