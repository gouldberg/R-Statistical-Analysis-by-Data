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
# Random intercept and slope model
# ------------------------------------------------------------------------------

# We want to know the relationship between the Shannon index and Percentage of Herb Layer Cover to fluctuate at random among plots ?


M6_0 <- lmer(Hlog10 ~ HerbLayerc + (1  | fPlot), data = Spiders, REML = FALSE)


# This model does not fit "bouundary (singular) fit"
M6_1 <- lmer(Hlog10 ~ HerbLayerc + (1 + HerbLayerc | fPlot), data = Spiders, REML = FALSE)
M6_1 <- lmer(Hlog10 ~ (1 + HerbLayerc | fPlot), data = Spiders, REML = FALSE)


summary(M6_1)


anova(M6_0, M6_1)

AIC(M6_0, M6_1)
