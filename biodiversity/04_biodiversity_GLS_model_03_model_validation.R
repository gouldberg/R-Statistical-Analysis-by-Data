setwd("//media//kswada//MyFiles//R//biodiversity")

packages <- c("dplyr", "lattice", "nlme")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Data:  Biodiversity
# ------------------------------------------------------------------------------

Biodiv <- read.table(file = "Biodiversity.txt", header = TRUE)

str(Biodiv)



# ----------
Biodiv$fTreatment <- factor(Biodiv$Treatment)

Biodiv$fNutrient <- factor(Biodiv$Nutrient)



# ------------------------------------------------------------------------------
# Model validation:  residuals plot
# ------------------------------------------------------------------------------
E <- resid(MFinal, type = "normalized")

Fit = fitted(MFinal)



op <- par(mfrow=c(1,2))
plot(x = Fit, y = E, 
     xlab="Fitted values",ylab="Residuals",
     main="Residuals versus fitted values")

# identify command allows us to identify the observation with the large residuals
# identify(Fit, E)

hist(E, nclass = 15)
par(op)




# ------------------------------------------------------------------------------
# Model validation:  different standard deviations
# ------------------------------------------------------------------------------
summary(MFinal)


# -->
# The information on the different standard deviations (multiplication factors of sigma) is interesting,
# as it shows the different variances (or better: the ratio with the standard error) per treatment - nutrient combination
# The estimated value for sigma is 0.819.
# Note that the combination enrich ment with algae and NH4 has the largest variance, namely (8.43 * 0.819)^2



# ------------------------------------------------------------------------------
# Model validation:  residuals and fitted values
# ------------------------------------------------------------------------------
par(mfrow=c(1,2))
boxplot(E ~ fTreatment * fNutrient, data = Biodiv)
boxplot(predict(MFinal) ~ fTreatment * fNutrient, data = Biodiv)


# -->
# Note the effect of observation 26 !
# We suggest that you repeat the entire analysis without this observation.



# ------------------------------------------------------------------------------
# main effect
# ------------------------------------------------------------------------------
graphics.off()

plot(effects::Effect("fNutrient", MFinal), lwd = 3, ci.style = "bands")

plot(effects::Effect("fTreatment", MFinal), lwd = 3, ci.style = "bands")




# ------------------------------------------------------------------------------
# effects of all higher-order terms
# ------------------------------------------------------------------------------
graphics.off()

# allEffects() calculates the effects for all high-order terms in a given model.
plot(effects::allEffects(MFinal), lwd = 3, ci.style = "bands")




