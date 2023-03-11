setwd("//media//kswada//MyFiles//R//presex")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PreSex
# ------------------------------------------------------------------------------
data("PreSex", package = "vcd")

str(PreSex)

dim(PreSex)


PreSex



# ----------
# Order variables G, P, E, M
tab <- aperm(PreSex, 4:1)



# ------------------------------------------------------------------------------
# Causal modeling
#  - The sequence of models of joint independence has another interpretation when the ordering of the variables is based on a set of ordered hypotheses
#    involving causal relationships among variables.
#  - Suppose, for example, that the causal ordering of 4 variables is A --> B --> C --> D, where the arrow means "is antecedent to".
#    Goodman suggests that the conditional joint probabilities of B, C, and D given A can be characterized by a set of recursive logit models
#    that treat (a) B as a response to A, (b) C as a response to A and B jointly, and (c) D as a response to A, B and C.
#  - These are equivalent to the loglinear models that we fit as the sequential baseline models of joint independence, namely [A][B], [AB][C], and [ABC][D].
#    The combination of these models with the marginal probabilities of A gives a characterization of the joint probabilities of all 4 variables.
#    In application, residuals from each submodel show the associations that remain unexplained.
# ------------------------------------------------------------------------------

# ----------
# Treat P as a response to G and examines the [Gender][Pre] mosaic to assess whether gender has an effect on premarital sex.
mosaic(margin.table(tab, 1:2), shade = TRUE, main = "Gender and Premarital Sex")

mod1 <- loglm(~ Gender + PremaritalSex, data = margin.table(tab, 1:2))

loddsratio(margin.table(tab, 1:2), stratum = 1, log = FALSE)



# -->
# men are much more likely to report premarital sex than are women



# ----------
# Treat E as a response to G and P jointly; the mosaic for [Gender, Pre][Extra] shows whether extramarital sex is related to either gender or premarital sex
mosaic(margin.table(tab, 1:3), expected = ~ Gender * PremaritalSex + ExtramaritalSex, main = "Gender * Pre + ExtramaritalSex")

mod2 <- loglm(~ Gender * PremaritalSex + ExtramaritalSex, data = margin.table(tab, 1:3))

loddsratio(margin.table(tab, 1:3), stratum = 1, log = FALSE)


# -->
# Men and women who have reported premarital sex are far more likely to report extramarital sex than those who have not.
# the conditional odds ratio osf extramarital sex than those who have sex is nearly the same for both genders
# Thus, extramarital sex depends on premarital sex, but not on gender



# ----------
# Finally, the mosaic for [Gender, Pre, Extra][Marital] is examined for evidence of the dependence of marital status on the 3 previous variables jointly.
# These models are equivalent to the recursive logit models whose path diagram is G --> P --> E --> M
mosaic(tab, expected = ~ Gender * PremaritalSex * ExtramaritalSex + MaritalStatus, main = "Gender * Pre * Extra + MaritalStatus")

mod3 <- loglm(~ Gender * PremaritalSex * ExtramaritalSex + MaritalStatus, data = tab)



# ----------
# Complete independence model
mod4 <- loglm(~ Gender + PremaritalSex + ExtramaritalSex + MaritalStatus, data = tab)



# ----------
# Check the likelihood ratio G^2:  mod4 = mod1 + mod2 + mod3
mod1$lrt  # [G][P]       df = 1    G^2 = 75.259
mod2$lrt  # [GP][E]      df = 3    G^2 = 48.929
mod3$lrt  # [GPE][M]     df = 7    G^2 = 107.956
mod4$lrt  # [G][P][E][M]  df = 11  G^2 = 232.142




# ------------------------------------------------------------------------------
# Final model
# ------------------------------------------------------------------------------
# Extramarital sex depends on premarital sex, but not on gender
mosaic(tab, expected = ~ Gender * PremaritalSex * ExtramaritalSex + MaritalStatus * PremaritalSex * ExtramaritalSex, main = "G*P*E + P*E*M")

mod5 <- loglm(~ Gender * PremaritalSex * ExtramaritalSex + MaritalStatus * PremaritalSex * ExtramaritalSex, data = tab)

mod5



