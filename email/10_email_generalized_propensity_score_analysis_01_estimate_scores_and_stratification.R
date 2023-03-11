# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\algebra_nation")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Define model and regression
# ------------------------------------------------------------------------------
  

# define model:  treat ~ covariate

formula1 <- formula("treatment ~ history + recency + channel")




# ----------
# fit regression model for treatment doses

mod <- lm(formula = formula1, data = biased_data)



summary(mod)



car::vif(mod)



car::residualPlots(mod)




# ------------------------------------------------------------------------------
# Estimate generalized propensity score
# ------------------------------------------------------------------------------


# conditional density of fitted(mod)

biased_data$GPS <- dnorm(biased_data$treatment, 
                  mean = fitted(mod), sd = sd(biased_data$treatment))



car::densityPlot(biased_data$GPS)




# ------------------------------------------------------------------------------
# GPS distribution by strata
# ------------------------------------------------------------------------------

biased_data$strataGPS <- with(biased_data, cut(GPS, include.lowest = T, labels = 1:5,
                                breaks = quantile(GPS, probs = seq(0, 1, 0.2))))


lattice::histogram(~ GPS | strataGPS, data = biased_data)



with(biased_data, by(GPS, strataGPS, summary))


