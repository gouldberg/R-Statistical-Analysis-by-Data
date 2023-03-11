setwd("//media//kswada//MyFiles//R//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------
Owls <- read.table(file = "//media//kswada//MyFiles//references//ZuurDataMixedModelling//Owls.txt", header = TRUE)

str(Owls)

Owls$LogNeg <- log10(Owls$NegPerChick + 1)



# ------------------------------------------------------------------------------
# Impose correlation structure explicitly:  correlation = corCompSymm(form = ~ 1 | nest)  --> cor(eis, eit) = Rho
#   - Recall that we used nest as a random intercept, and therefore, the compound correlation structure was imposed on the
#     observations from the same nest. We can get the same correlation structure (and estimated parameters) by specifying this correlation structure
#     explicitly.
# ------------------------------------------------------------------------------

library(nlme)

Form <- formula(LogNeg ~ SexParent * FoodTreatment + SexParent * ArrivalTime)

M2.gls <- gls(Form, correlation = corCompSymm(form = ~ 1 | Nest), method="REML", data = Owls)


summary(M2.gls)



# -->
# Estimated correlation (Rho) is 0.138.
# Hence, the correlation between any two observations from the same nest i is given by cor(eis, eit) = 0.138

# It is important to realise that both random intercept and compound correlation models assume that the correlation coefficient between
# any two observations from the same nest are equal, whether the time difference is 5 minutes or 5 hours.


