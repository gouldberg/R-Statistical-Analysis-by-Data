setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)
str(Vietnam)


car::some(Vietnam)


# frequency form in dataframe to table
( vietnam.tab <- xtabs(Freq ~ sex + year + response, data = Vietnam) )

str(vietnam.tab)



# ------------------------------------------------------------------------------
# Fitting models
#  - complete (mutual) independence:  all joint probabilities are products of the 1-way marginal probabilities:   [R][Y][S]  (~ response + year * sex)
#  - joint independence:              response is jointly independent of year and sex:             [R][YS]  (~ response + year * sex)
#  - conditional independence:        response and year are conditionally independent given sex:   [RS][YS]  (~ response * sex + year * sex)
#  - No 3-way interaction:            no pair is marginally or conditionally independent:     [RS][YS][RY]  (~ response * sex + year * sex + response * year)
# ------------------------------------------------------------------------------

#  --> The pattern of residuals in the mosaic will suggest associations to be added to an adequate explanatory model.
# As the model achieves better fit to the data, the degree of shading decreases, so we may think of the process of model fitting as "clearning the mosaic"

abbrev <- list(abbreviate = c(1, FALSE, FALSE))

# complete mutual independence
vcd::mosaic(vietnam.tab, expected = ~ response + year + sex, labeling_args = abbrev, main = "Model: ~ response + year + sex")


# joint independence:  base hypothesis here
vcd::mosaic(vietnam.tab, expected = ~ year * sex + response, labeling_args = abbrev, main="Model: ~ year * sex + response")


# conditional independence:  the response versus year separately for males and females
vcd::mosaic(vietnam.tab, expected = ~ response * sex + year * sex, labeling_args = abbrev, main="Model: ~ response * sex + year * sex")


# No 3-way interaction
vcd::mosaic(vietnam.tab, expected = ~ response * sex + year * sex + response * year, labeling_args = abbrev, main="Model: ~ No 3-way interaction")


# all residuals are zero
vcd::mosaic(vietnam.tab, expected = ~ response * year * sex, labeling_args = abbrev, main="Model: ~ 3-way interaction")



# ----------
# complete (mutual) independence
mod1 <- MASS::loglm(~ response + year + sex, data = vietnam.tab)


# joint independence
mod2 <- MASS::loglm(~ year * sex + response, data = vietnam.tab)


# conditional independence
mod3 <- MASS::loglm(~ response * sex + year * sex, data = vietnam.tab)


# No 3way interaction
mod4 <- MASS::loglm(~ response * sex + year * sex + response * year, data = vietnam.tab)


# 3way interaction
mod5 <- MASS::loglm(~ response * year * sex, data = vietnam.tab)



# ----------
vcdExtra::LRstats(mod1, mod2, mod3, mod4, mod5)



# -->
# mod4 is best in terms of AIC and BIC (except for mod5), but still there are residuals



# ----------
# Alternatively you can get the Pearson and likelihood ratio (LR) tests for a given model using anova()
anova(mod3)



# ----------
# or compare a set of models using LR tests on the difference in LR X^2 from one model to the next, when a list of models is supplied to anova()
anova(mod1, mod2, mod3, mod4, mod5, test = "chisq")

