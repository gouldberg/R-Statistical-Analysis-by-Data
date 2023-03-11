# setwd("//media//kswada//MyFiles//R//waiting_children//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//waiting_children//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  waiting children
#  - number of children and number of waiting children at 47 prefectures in 2014
# ------------------------------------------------------------------------------

# number of prefectures (data length)
N <- 47


# number of waiting children
x <- c(64,0,139,408,53,0,171,227,66,0,658,889,8672,880,17,0,
       0,0,0,0,27,96,107,48,372,11,461,349,76,5,0,3,
       23,0,57,41,0,0,3,306,50,1,359,0,0,185,1721)


# number of children
S <- c(40383,26285,21664,18573,18588,22094,18068,48652,24397,28400,76031,
       58988,211883,33504,43249,20155,25899,26095,21901,52025,41731,33543,
       95996,41427,23121,27009,60241,42512,19206,16064,17659,21707,17114,
       26789,20652,16213,12745,19967,12711,49827,21297,23912,32543,15030,
       18571,24963,30959)



# ----------
data <- data.frame(x = x, S = S) %>% mutate(ratio = x / S)




# ------------------------------------------------------------------------------
# Fit a set of predetermined distributions to the data and choose the best
#   - fitDist() uses gamlssML() to fit a set of predetermined distributions to the data
#     and choose the best according to the GAIC, with default penalty k = 2
# ------------------------------------------------------------------------------

# All of the gamlss.family distributions on the real line have been considered

f1 <- fitDist(data$ratio + 0.0000001, type = "real0to1", k = 2, trace = TRUE)



# ----------
# best model is BE and BEo
f1$fits


f1$failed



# ------------------------------------------------------------------------------
# Residuals diagnostics
# ------------------------------------------------------------------------------

wp(f1, ylim.all = 0.5)



# -->
# but really bad..



# ------------------------------------------------------------------------------
# Plot best model fitting
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

histDist(data$ratio + 0.0000001, family = BE)

