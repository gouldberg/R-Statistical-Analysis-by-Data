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
# 2-Stage Analysis Method
#   - a linear regression model is applied on data of one beach, and this process is then carried out for data of each beach in turn.
#   - The 2-stage analysis has various disadvantages.
#       - we summarise all the data from a beach with one parameter
#       - in the 2nd step, we analyse regression parameters, not the observed data, hence we are not modelling the variable of interest directly.
#       - finally, the number of observations used to calculate the summary statistic is not used in the 2nd step.
# ------------------------------------------------------------------------------

Beta <- vector(length = 9)

for (i in 1:9){
  tmpout <- summary(lm(Richness ~ NAP, subset = (Beach == i), data = RIKZ))
  Beta[i] <- tmpout$coefficients[2,1]
}

Beta



# -->
# Note that there are considerably differences in the 9 estimated slopes for NAP.



# ----------
# In the second step, the estimated regression coefficients are modelled as a function of exposure.
# This is just a one-way ANOVA.

ExposureBeach <- c("a","a","b","b","a","b","b","a","a")

tmp2 <- lm(Beta ~ factor(ExposureBeach))

summary(tmp2)



# -->
# p-value for exposure is 0.22, indicating that there is no significant exposure effect on the nine slopes.
# Here we are not modelling the variable of interest directly.
