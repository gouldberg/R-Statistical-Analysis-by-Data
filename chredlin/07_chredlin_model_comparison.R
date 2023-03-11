setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ----------
linmod <- lm(involact ~ race + fire + theft + age + log(income), data = chredlin)

linmod2 <- lm(involact ~ -1 + race + fire + age + theft, data = chredlin)


library(mgcv)
twmod <- gam(involact ~ race + fire + theft + age + log(income), family = tw(link = "log"), data = chredlin)




# ------------------------------------------------------------------------------
# model comparison: terms and coefficient
# ------------------------------------------------------------------------------

stargazer::stargazer(linmod, linmod2, twmod, type = "text")






# ------------------------------------------------------------------------------
# model comparison by likelihood-ratio tests and the analysis of variance:  ONLY APPLICABLE TO NETSTED MODELS
# ------------------------------------------------------------------------------


# intercept only model
linmod0 <- update(linmod, . ~ 1)

summary(linmod0)



# ----------
# linmod is better than intercept only model ?  --> better

anova(linmod0, linmod)



# ----------
# linmod2 is better than linmod ?  --> not better

anova(linmod, linmod2)




# ------------------------------------------------------------------------------
# model comparison by AIC:  APPLICABLE TO NON-NESTED MODELS
# ------------------------------------------------------------------------------

AIC(linmod, linmod2, twmod)



# -->
# linmod2 is best ..



