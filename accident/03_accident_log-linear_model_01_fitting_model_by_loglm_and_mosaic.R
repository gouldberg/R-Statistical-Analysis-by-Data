setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)


# frequency form in dataframe to table
( acci.tab <- xtabs(Freq ~ age + result + mode + gender, data = Accident) )



# ------------------------------------------------------------------------------
# Fitting models
#  - complete (mutual) independence:  all joint probabilities are products of the 1-way marginal probabilities:  [A][M][G][R]  (~ age + mode + gender + result)
#  - joint independence:              result is jointly independent of age, mode and gender:  [R][AMG]  (~ result + age * mode * gender)
#  - conditional independence:        result and mode are conditionally independent given age and gender:  [RAG][MAG]  (~ result * age * gender + mode * age * gender)
#  - No 4-way interaction:            no pair is marginally or conditionally independent:  --> not define here
# ------------------------------------------------------------------------------

#  --> The pattern of residuals in the mosaic will suggest associations to be added to an adequate explanatory model.
# As the model achieves better fit to the data, the degree of shading decreases, so we may think of the process of model fitting as "clearning the mosaic"

abbrev <- list(abbreviate = c(TRUE, TRUE, TRUE, TRUE))

# complete mutual independence
vcd::mosaic(acci.tab, expected = ~ age + mode + gender + result, labeling_args = abbrev, main = "Model: ~ age + mode + gender + result")


# joint independence:  base hypothesis
vcd::mosaic(acci.tab, expected = ~ result + age * mode * gender, labeling_args = abbrev, main="Model: ~ result + age * mode * gender")


# conditional independence
vcd::mosaic(acci.tab, expected = ~ result * age  * gender + mode * age * gender, labeling_args = abbrev, main="Model: ~ result * age * gender + mode * age * gender")




# ----------
# complete (mutual) independence
mod1 <- MASS::loglm(~ age + mode + gender + result, data = acci.tab)


# joint independence
mod2 <- MASS::loglm(~ result + age * mode * gender, data = acci.tab)


# conditional independence
mod3 <- MASS::loglm(~ result * age * gender + mode * age * gender, data = acci.tab)




vcdExtra::LRstats(mod1, mod2, mod3)



# -->
# mod3 is best in terms of AIC and BIC, but still there are residuals



# ----------
# Alternatively you can get the Pearson and likelihood ratio (LR) tests for a given model using anova()
anova(mod3)



# ----------
# or compare a set of models using LR tests on the difference in LR X^2 from one model to the next, when a list of models is supplied to anova()
anova(mod1, mod2, mod3, test = "chisq")

