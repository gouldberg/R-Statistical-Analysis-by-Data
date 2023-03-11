setwd("//media//kswada//MyFiles//R//tobinq")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TobinQ
# ------------------------------------------------------------------------------

data("TobinQ", package = "pder")

str(TobinQ)

dim(TobinQ)



# ------------------------------------------------------------------------------
# Cluster-robust standard errors
# ------------------------------------------------------------------------------

# standard SE
coeftest(Q.within)


# cluster-robust standard errors
coeftest(Q.within, vcov = vcovHC(Q.within), type = "HCO", cluster = "state")


# -->
# Note that the standard errors are not much different



# ------------------------------------------------------------------------------
# Individual effects
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(fixef(Q.within, tyoe = "level"), type = "h", xlab = "cusip", ylab = "", main = "individual intercepts")
plot(fixef(Q.within, type = "dfirst"), type = "h", xlab = "cusip", ylab = "", main = "individual effects in deviations from the 1st individual" )
plot(fixef(Q.within, type = "dmean"), type = "h", xlab = "cusip", ylab = "", main = "individual effects in deviations from mean")



