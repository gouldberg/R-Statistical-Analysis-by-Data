# setwd("//media//kswada//MyFiles//R//rikz")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//rikz")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
# ------------------------------------------------------------------------------

RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)


str(RIKZ)


car::some(RIKZ)



# ----------
RIKZ$fBeach <- factor(RIKZ$Beach)

Mlme1 <- lme(Richness ~ NAP, random = ~1 | fBeach, data = RIKZ)

summary(Mlme1)



mod_obj <- Mlme1



# ------------------------------------------------------------------------------
# model understanding:  interclass correlation
# ------------------------------------------------------------------------------

summary(mod_obj)


# StdDev for the random intercept 
d <- 2.94


# StdDev for the residual
sigma <- 3.06


d^2

d^2 + sigma^2



# induced correlation (or interclass correlation):  The correlation between two observations from the same beach
( rho <- d^2 / (d^2 + sigma^2) )


# -->
# induced correlation is 0.48, which is relatively high.



# ------------------------------------------------------------------------------
# model understanding:  design effect
#   - standard error = standard deviation / sqrt(sample size)
#     but if the data are nested (hierarchical), "design effect" should be taken into account.
#     "design effect" indicates how much the denominator should be adjusted.
# ------------------------------------------------------------------------------

# for a 2-stage design with equal number of samples per beach (n = 5) and interclass correlation rho,
# the design effect is
n <- 5

( design_effect <- 1 + (n - 1) * rho )


# -->
# This number if larger than 1, and in this case it is 2.92,
# we should not use sqrt(sample size) as denomitor for estimation of standard errors.



# ------------------------------------------------------------------------------
# model understanding:  effective sample size
# ------------------------------------------------------------------------------

N <- 9

( effetive_sample_size <- N * n / design_effect )


# -->
# standard error = standard deviation / sqrt(effective_sample_size)
# so a high intraclass correlation (0.48) means that the corrected sample size is considerable lower, and this means less precise standard errors !

