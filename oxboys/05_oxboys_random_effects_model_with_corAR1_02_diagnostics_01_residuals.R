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


m0_2 <- lme(height ~ age + I(age^2), data = d, 
            random = list(Subject = ~ age),
            correlation = corAR1(form = ~ age | Subject), control = lmc)


mod_obj <- m0_2




# ------------------------------------------------------------------------------
# Standardized residuals against fitted value and age, with by group (Subject)
# ------------------------------------------------------------------------------

plot(mod_obj)


plot(mod_obj, resid(., type = "p") ~ fitted(.) | Subject, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)

plot(mod_obj, resid(., type = "p") ~ age | Subject, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)



# -->
# This is bad model ...




# ------------------------------------------------------------------------------
# Distribution of Standardized residuals by group (Seed)
# ------------------------------------------------------------------------------

plot(mod_obj, Subject ~ resid(., type = "p"))


