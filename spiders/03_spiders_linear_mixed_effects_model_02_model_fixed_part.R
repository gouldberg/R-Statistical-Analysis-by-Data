setwd("//media//kswada//MyFiles//R//spiders")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Spiders
# ------------------------------------------------------------------------------

Spiders <- read.table(file = "Spiders.txt", header = TRUE, dec = ".")


str(Spiders)



# ----------
# Some plots are dropped from the analysis
Spiders$fPlot <- factor(Spiders$Plot)

Spiders <- Spiders %>% filter(! fPlot %in% c("4", "9", "11", "14", "23"))

Spiders$fPlot <- as.factor(as.numeric(Spiders$fPlot))



# ------------------------------------------------------------------------------
# Standardizing covariate (to compare with MCMC result)
# ------------------------------------------------------------------------------

# Add na.rm = TRUE if need to deal with NAs
MyNorm <- function(x){ (x - mean(x)) / sd(x) }

Spiders$HerbLayerc <- MyNorm(Spiders$HerbLayer)
Spiders$GroundVegc <- MyNorm(Spiders$GroundVeg)
Spiders$Litterc    <- MyNorm(Spiders$Litter)



# ------------------------------------------------------------------------------
# Model fixed part:  including Percentage of Herb Layer Cover, Percentage of Ground Vegetation, and Percentage of Litter Content.
# ------------------------------------------------------------------------------

M3 <- lmer(Hlog10 ~ HerbLayerc + GroundVegc + Litterc + (1 | fPlot),  data = Spiders)


summary(M3)


# -->
# GroundVegc is not significant



# ------------------------------------------------------------------------------
# Model fixed part
# Compute normalized residuals and fitted values
# ------------------------------------------------------------------------------

# normalized residuals (type = "n") = residulas divided by the square root of the variance
# E3 <- resid(M3)
E3 <- resid(M3, type = "n")



# ----------
# fitted values
F3 <- fitted(M3)



# ----------
# Fitted values and residuals by manually computing
Betas     <- fixef(M3)
X         <- model.matrix(M3)

RE <- ranef(M3)$fPlot$'(Intercept)'
AllRE <- RE[as.numeric(Spiders$fPlot)]

FitManual <- X %*% Betas + AllRE


# check
head(F3)
head(FitManual)

FitManual - F3
resid(M3) - ( Spiders$Hlog10 - FitManual )


