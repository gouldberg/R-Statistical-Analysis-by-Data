setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\hurricanes")

packages <- c("dplyr")

purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hurricanes
# ------------------------------------------------------------------------------

# data("Hurricanes", package = "rethinking")

data <- read.csv(file = "Hurricanes.txt", header = T, sep = "\t")


dim(data)


str(data)



car::some(data)




# ------------------------------------------------------------------------------
# Poisson regression model and Quassi-Poisson model
# ------------------------------------------------------------------------------


mod_p <- glm(deaths ~ min_pressure + damage_norm + femininity, data = data, family = "poisson")




# ----------
par(mfrow = c(2,2))

plot(mod_p)




# ----------
car::residualPlots(mod, groups = data$female)




# ----------
# dispersion parameter:  residual deviance devided by df.
with(mod_qp, deviance / df.residual)




# ----------
# Pearson estimate of dispersion
( phi <- sum(residuals(mod_p, type = "pearson")^2 / mod_p$df.residual) )


# standard errors of coefficients in this model should be multiplied by, to correct for overdispersion
sqrt(phi)



# -->
# Indicates that standard errors of coefficients in this model should be multipled by 7.24, a 624% increase to correct for overdispersion
# There is substantial overdispersion in the data,
# possible to the extent that even a negative binomial GLM will not sufficiently adjust for the variation.



# ----------
summary(mod_p, dispersion=phi)




# -->
# now the femininity is not significant.

