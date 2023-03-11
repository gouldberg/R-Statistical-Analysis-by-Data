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
# Non-proportional odds model
# ------------------------------------------------------------------------------


debt$ccarduse <- factor(debt$ccarduse, levels = c(1, 2, 3), ordered = TRUE)


library(VGAM)


nmodi <- vglm(ccarduse ~ incomegp + house + agegp + bankacc + bsocacc + cigbuy + locintrn + prodebt, family = cumulative(parallel = FALSE), debt)


nmodi


summary(nmodi)



# ----------
# for comparison, proportional odds model by vglm
pomodi_2 <- vglm(ccarduse ~ incomegp + house + agegp + bankacc + bsocacc + cigbuy + locintrn + prodebt, family = cumulative(parallel = TRUE), debt)



# ----------
1 - pchisq(deviance(pomodi_2) - deviance(nmodi), 1)



# -->
# There is a difference of only one parameter between the two models.
# We see that the simplification to the proportional odds model IS justified here.


