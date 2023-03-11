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



# ----------
# vglm needs case form and ordered factor  ("weights" have different meaning)
Vietnam2 <- expand.dft(Vietnam)

Vietnam2$response <- ordered(Vietnam2$response)



# ------------------------------------------------------------------------------
# proportional odds model by VGAM
# ------------------------------------------------------------------------------

library(VGAM)


viet.po <- vglm(response ~ sex + year, data = Vietnam2, family = cumulative(parallel = TRUE))

viet.po

summary(viet.po)



# ----------
# VGAM package defines a coef() method that can print the coefficients in a more readable matrix form giving the category cutpoints
coef(viet.po, matrix = TRUE)



# ------------------------------------------------------------------------------
# non-proportional odds model by VGAM
# ------------------------------------------------------------------------------

viet.npo <- vglm(response ~ sex + year, data = Vietnam2, family = cumulative(parallel = FALSE))


viet.npo

summary(viet.npo)



# ----------
coef(viet.npo, matrix = TRUE)



# ------------------------------------------------------------------------------
# Compare nested model: LR test
# ------------------------------------------------------------------------------

# In most cases, nested models can be tested using an nova() method,
# but hte VGAM package has not implemented this for "vglm" objects.
# Instead, it provides an analogous function lrtest()

VGAM::lrtest(viet.npo, viet.po)



# ------------------------------------------------------------------------------
# LR test manually using the difference in residual deviance for the two models
# ------------------------------------------------------------------------------

tab <- cbind(Deviance = c(deviance(viet.npo), deviance(viet.po)),
             df = c(df.residual(viet.npo), df.residual(viet.po)))


tab <- rbind(tab, diff(tab))


rownames(tab) <- c("GenLogit", "PropOdds", "LR test")


tab <- cbind(tab, pvalue = 1 - pchisq(tab[,1], tab[,2]))


tab





