setwd("//media//kswada//MyFiles//R//hodges")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hodges
# ------------------------------------------------------------------------------

data("hodges", package = "gamlss.data")


str(hodges)

car::some(hodges)



# ------------------------------------------------------------------------------
# random intercept model by lme()
# ------------------------------------------------------------------------------

# REML
l1 <- lme(prind ~ 1, data = hodges, random = ~1 | state)


# ML
l2 <- lme(prind ~ 1, data = hodges, random = ~1 | state, method = "ML")


summary(l1)

summary(l2)



# ----------
plot(l1)


plot(l2)
