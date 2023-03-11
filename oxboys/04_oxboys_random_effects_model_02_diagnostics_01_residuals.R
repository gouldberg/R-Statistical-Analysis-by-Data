setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ----------
lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)

m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = d,
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)), control = lmc)


mod_obj <- m0



# ------------------------------------------------------------------------------
# Standardized residuals against fitted value and age, with by group (Subject)
# ------------------------------------------------------------------------------

plot(mod_obj)


plot(mod_obj, resid(., type = "p") ~ fitted(.) | Subject, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)

plot(mod_obj, resid(., type = "p") ~ age | Subject, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)



# -->
# The plot shows a clear trend in the mean of the redisuals ??
# This suggests a need for a more flexible model ??



# ------------------------------------------------------------------------------
# Distribution of Standardized residuals by group (Subject)
# ------------------------------------------------------------------------------

plot(mod_obj, Subject ~ resid(., type = "p"))


