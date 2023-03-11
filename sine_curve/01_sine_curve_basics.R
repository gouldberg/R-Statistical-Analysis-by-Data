setwd("//media//kswada//MyFiles//R//sine_curve")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: sine curve
# ------------------------------------------------------------------------------
set.seed(100)

x <- runif(100, min = 2, max = 10)

y <- sin(x) + rnorm(length(x)) * 0.25

( sinData <- data.frame(x = x, y = y) )


graphics.off();  par(mfrow=c(1,1))

plot(x, y)


