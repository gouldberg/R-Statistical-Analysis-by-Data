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
# Compare the results obtained with the 4 estiamtion methods
# ------------------------------------------------------------------------------

Q.walhus <- update(Q.swar, random.method = "swar")


Q.amemiya <- update(Q.swar, random.method = "amemiya")


Q.nerlove <- update(Q.swar, random.method = "nerlove")


Q.models <- list(swar = Q.swar, walhus = Q.walhus, amemiya = Q.amemiya, nerlove = Q.nerlove)



# ----------
sapply(Q.models, function(x) ercomp(x)$theta)


sapply(Q.models, coef)



# -->
# These are very close to each other, and consequently, the estimated coefficients for the 4 models are almost identical


