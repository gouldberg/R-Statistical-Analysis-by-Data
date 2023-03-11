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



# ------------------------------------------------------------------------------
# Poisson GLM:  only one covariate (MeanDepth)
# ------------------------------------------------------------------------------

M1 <- glm(TotAbund ~ MeanDepth, data = DF, family = poisson(link = "log"))

summary(M1)




# ------------------------------------------------------------------------------
# Poisson GLM:  adding Period covariate (with interaction)
# ------------------------------------------------------------------------------

M2 <- glm(TotAbund ~ MeanDepth * fPeriod,  data = DF,  family = poisson)


summary(M2)




# ------------------------------------------------------------------------------
# Poisson GLM:  Using the offset
#   - Counts are often considered as rates at which events occur within areas of different sizes or as time periods of differing duration.
#     If we wish to understand the Poisson mean as the rate at which events occur over given geographic areas or time periods,
#     we must adjust for the size of the area or time period in which events occur.
# ------------------------------------------------------------------------------

DF$LogSA <- log(DF$SweptArea)

M3 <- glm(TotAbund ~ MeanDepth * factor(Period) + offset(LogSA), data = DF,  family = poisson)


summary(M3)





