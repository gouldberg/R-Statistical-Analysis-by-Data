setwd("//media//kswada//MyFiles//R//debt")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  debt
# ------------------------------------------------------------------------------

data("debt", package = "faraway")


str(debt)


head(debt)



# ------------------------------------------------------------------------------
# Proportional Hazards Model
#   - Hazards of category j is the probability of falling in category j given that your category is greater than j.
#   - The link function is complementary log-log and the corresponding latent variable distribution is the extreme value
# ------------------------------------------------------------------------------

debt$ccarduse <- factor(debt$ccarduse, levels = c(1, 2, 3), ordered = TRUE)


# The extreme value distribution is not symmetric like the logistic and normal and so there seems little justification for
# applying it to this data, but try.

library(MASS)


phmod <- polr(ccarduse ~ ., data = na.omit(debt), method = "cloglog")


phmod



# ----------
c(deviance(pomod), pomod$edf)


c(deviance(phmod), phmod$edf)



# -->
# Proportional Hazards is not better than proportional odds model



# ------------------------------------------------------------------------------
# Model selection by AIC-based
# ------------------------------------------------------------------------------

phmodi <- step(phmod, direction = "both")


summary(phmodi)



# -->
# Now the house is included ...



# ----------
summary(pomodi)


summary(phmodi)




# ----------
c(deviance(pomodi), pomodi$edf)


c(deviance(phmodi), phmodi$edf)


pchisq(deviance(phmodi) - deviance(pomodi), phmodi$edf - pomodi$edf, lower = FALSE)



# -->
# proportional hazards model is marginally bettern than proportional odds model



AIC(phmodi, pomodi)

