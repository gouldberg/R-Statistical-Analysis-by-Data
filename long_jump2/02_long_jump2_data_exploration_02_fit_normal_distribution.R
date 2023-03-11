setwd("//media//kswada//MyFiles//R//long_jump2")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  long jump2
# ------------------------------------------------------------------------------

N <- 20

x <- c(775, 779, 799, 794, 770, 790, 775,  
     778, 808, 802, 776, 775, 799, 787,
     825, 785, 775, 762, 782, 788)


data <- list(N = N, x = x)



# ------------------------------------------------------------------------------
# data exploration:  fit Normal distribution
# ------------------------------------------------------------------------------

library(gamlss)

graphics.off()

gamlss::histDist(x, density = TRUE, family = NO)



# -->
# Mu = 786.2
# Sigma = 2.677

