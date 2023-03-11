# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ----------
M1 <- glm(TotAbund ~ MeanDepth, data = DF, family = poisson(link = "log"))

M2 <- glm(TotAbund ~ MeanDepth * fPeriod,  data = DF,  family = poisson)

DF$LogSA <- log(DF$SweptArea)

M3 <- glm(TotAbund ~ MeanDepth * factor(Period) + offset(LogSA), data = DF,  family = poisson)



# ------------------------------------------------------------------------------
# Model Comparison by Likelihood Ratio Test
# ------------------------------------------------------------------------------

lmtest::lrtest(M1, M2, M3)


# -->
# M3 is significantly better than M2



# ----------
# We can caompre a given Poisson model to its negative-binomial counterpart, where are nested
lmtest::lrtest(M3, M3nbin)




# ------------------------------------------------------------------------------
# Compare AIC and BIC
# ------------------------------------------------------------------------------

vcdExtra::LRstats(M1, M2, M3, sortby = "BIC")



# -->
# M2 is best in AIC and BIC
# M3 is NOT best ...





