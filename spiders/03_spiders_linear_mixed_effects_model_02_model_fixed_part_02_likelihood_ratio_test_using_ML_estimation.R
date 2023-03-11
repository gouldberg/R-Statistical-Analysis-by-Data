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
# Model fixed part
# Likelihood ratio test using ML estimation
# ------------------------------------------------------------------------------

# Instead of using the t-values, we can apply a likelihood ratio test based on ML estimation.

M4 <- lmer(Hlog10 ~ HerbLayerc + GroundVegc + Litterc + (1 | fPlot), data = Spiders, REML = FALSE)



# ----------
M4A <- update(M4, .~. - HerbLayerc)

M4B <- update(M4, .~. - GroundVegc)

M4C <- update(M4, .~. - Litterc)


anova(M4, M4A)

anova(M4, M4B)

anova(M4, M4C)



# ----------
# Do the same thing
# Keep in mind that if the sample size minus the number of parameters in relatively small (approximately < 50), these reported @p-values are unreliable
# This is not the case here.
drop1(M4, test = "Chi")



# ----------
AIC(M4, M4A, M4B, M4C)



# -->
# Indicating that Percentage of Ground Vegetation can be dropped (the lower the AIC the better).
# Alternatively, an information theoretic approach can be adopted. We decide not to apply model selection, and we keep all covariates in the model.
