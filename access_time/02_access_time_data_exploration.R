# setwd("//media//kswada//MyFiles//R//access_time//")
# setwd("//media//kswada//MyFiles//R//Bayesian_inference//access_time//")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\access_time")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  access time
# ------------------------------------------------------------------------------

data <- read.csv("access_time.csv", header = T)


str(data)




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


# This is time data within day

hist(data$x, breaks = seq(0, 24, by = 0.25))



# ----------
abline(v = c(8.87, 11.94, 17.3, 22.35), lty = 2, col = "blue")

