setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")


TV

dim(TV)

str(TV)



# ----------
TV2 <- margin.table(TV, c(1,3))

TV2



# ----------
TV.df <- as.data.frame.table(TV)

levels(TV.df$Time) <- rep(c("8", "9", "10"), c(4, 4, 3))

TV3 <- xtabs(Freq ~ Day + Time + Network, TV.df)

TV3




# ------------------------------------------------------------------------------
# Fitting models
#  - complete (mutual) independence:  all joint probabilities are products of the 1-way marginal probabilities:   [Day][Time][Network]  (~ Day + Time + Network)
#  - joint independence:              Time is jointly independent of Day and Network:             [DayNetwork][Time]  (~ Day * Network + Time)
#  - conditional independence:        Day and Time are conditionally independent given Network:   [DayNetwork][TimeNetwork]  (~ Day * Network + Time * Network = ~ (Day + Time) * Network)
#  - No 3-way interaction:            no pair is marginally or conditionally independent:     [DayTime][DayNetwork][TimeNetwork]  (~ Day * Time + Day * Network + Time * Network)
# ------------------------------------------------------------------------------

#  --> The pattern of residuals in the mosaic will suggest associations to be added to an adequate explanatory model.
# As the model achieves better fit to the data, the degree of shading decreases, so we may think of the process of model fitting as "clearning the mosaic"

abbrev <- list(abbreviate = c(1, FALSE, 1))


# complete (mutual) independence:  [Day][Time][network]
vcd::mosaic(TV3, expected = ~ Day + Time + Network, labeling_args = abbrev, main = "Model: ~ Day + Time + Network")


# joint independence:  [Day Network][Time]
vcd::mosaic(TV3, expected = ~ Day * Network + Time, labeling_args = abbrev, main="Model: ~ Day * Netowrk + Time")


# conditional independence:  [Day Network][Time Network]
vcd::mosaic(TV3, expected = ~ Day * Network + Time * Network, labeling_args = abbrev, main="Model: ~ Day * Netowrk + Time * Network")


# No 3-way interaction:  [Day Time][Day Network][Time Network]
vcd::mosaic(TV3, expected = ~ Day * Time + Day * Network + Time * Network, labeling_args = abbrev, main="Model: ~ No 3-way interaction")


# all residuals are zero
vcd::mosaic(TV3, expected = ~ Day * Time * Network, labeling_args = abbrev, main="Model: ~ 3-way interaction")




# ------------------------------------------------------------------------------
# Fitting models by loglm
# ------------------------------------------------------------------------------

mod1 <- MASS::loglm(~ Day + Time + Network, data = TV3)


# joint independence
mod2 <- MASS::loglm(~ Day * Network + Time, data = TV3)

mod2_2 <- MASS::loglm(~ Time * Network + Day, data = TV3)


# conditional independence
mod3 <- MASS::loglm(~ Day * Network + Time * Network, data = TV3)


# No 3way interaction
mod4 <- MASS::loglm(~ Day * Time + Day * Network + Time * Network, data = TV3)


# 3way interaction
mod5 <- MASS::loglm(~ Day * Time * Network, data = TV3)



# ----------
vcdExtra::LRstats(mod1, mod2, mod2_2, mod3, mod4, mod5)



# -->
# mod4 is best in terms of AIC and BIC (except for mod5), but still there are residuals



# ----------
# Alternatively you can get the Pearson and likelihood ratio (LR) tests for a given model using anova()
anova(mod4)



# ----------
# or compare a set of models using LR tests on the difference in LR X^2 from one model to the next, when a list of models is supplied to anova()
anova(mod1, mod2_2, mod3, mod4, mod5, test = "chisq")

