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



# ----------
mod_obj <- M3



# ------------------------------------------------------------------------------
# Model validation:  Assess spatial dependence in the Pearson residuals, check spatial positioning of the sites before adding covariate Period
# ------------------------------------------------------------------------------

# We have additional covariates that could be added to the model, the categorical variable Period, for one.
# We can also add the interaction between Mean Depth and Period.

Mypch <- DF$Period

Mypch[DF$Period == 2] <- 16

xyplot(Ykm ~ Xkm, aspect = "iso", data = DF, pch = Mypch, col = 1, xlab = "X-coordinate", ylab = "Y-coordinate")


