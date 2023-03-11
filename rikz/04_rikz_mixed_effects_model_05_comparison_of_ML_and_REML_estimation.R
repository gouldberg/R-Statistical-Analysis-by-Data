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



# ------------------------------------------------------------------------------
# Illustration of difference between ML and REML
# ------------------------------------------------------------------------------

RIKZ$fExp <- RIKZ$Exposure

RIKZ$fExp[RIKZ$fExp == 8] <- 10

RIKZ$fExp <- factor(RIKZ$fExp, levels=c(10,11))

M0.ML <- lme(Richness ~ NAP, data = RIKZ, random = ~1 | fBeach, method = "ML")

M0.REML <-lme(Richness ~ NAP, data = RIKZ, random = ~ 1 | fBeach, method = "REML")

M1.ML <- lme(Richness ~ NAP + fExp, data = RIKZ, random = ~ 1 | fBeach, method = "ML")

M1.REML <- lme(Richness ~ NAP + fExp, data = RIKZ, random = ~ 1 | fBeach, method = "REML")


summary(M0.ML)

summary(M0.REML)



# -->
# absolute value of correlation of intercepts is smaller fro REML

