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
# Linear mixed effects model
#   - Treating Plot as a fixed effect means that the Shannon index/covariate relationships found by the model applies only to those 26 plots
#     If Plot is treated as a random component, the Shannon index/covariate relationship will be applicable to all plots, not just the 26 used in the analysis.
#     This means that we need to assume that the 26 sampled plots represent the population of all plots with similar habitat conditions.
#     If we cannot make this assumption, we must treat them as fixed effects.
#   - Also, another important point to consider is the number of levels. 
#     If there a sufficient nuber of levels (>4 and preferably > 10) in the variable, treat it as a random effect.
#     In cases in which an explanatory variable is a treatment effect, consider it a fixed effect, since, in most situations, 
#     the primary focus of an experiment is the effects of the treatment.
# ------------------------------------------------------------------------------

library(lme4)

M2 <- lmer(Hlog10 ~ HerbLayer + (1 | fPlot),  data = Spiders)

summary(M2)



# ------------------------------------------------------------------------------
# Compute p-value for fixed effects coefficient
# ------------------------------------------------------------------------------

# Note that the summary function applied to the lmer object M2 does not give p-values for the t-values.
# The omission is due to an ongoing debate in the mixed-modelling fied on whether the t-statistic follows a t-distribution (The F test is similarly debated).
# Pinheiro and Bates (2000) show that, if the sample size minus the number of parameters is small, the p-values produced are unreliable

# Here calculate p-value based on a z-distribution

Betas  <- fixef(M2)                  

SE     <-  sqrt(diag(vcov(M2)))

pval   <- 2 * pnorm(-abs(Betas / SE))

Output <- cbind(Betas, SE, pval)

print(Output, digits = 3)



# -->
# The fact that we now have p-values does not mean that we can trust them.
# Instead of relying on the p-value, we can create a 95% confidence interval and see whether 0 falls within it.
# Take the estimated value and add/sutract 1.96 * the standard error



# ------------------------------------------------------------------------------
# Compute 95% confidence interval
# ------------------------------------------------------------------------------
data.frame(Output) %>% mutate(upper = Betas + 1.96 * SE, lower = Betas - 1.96 * SE)


# -->
# The upper and lower does not include zero.



