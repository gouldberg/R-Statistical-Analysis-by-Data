setwd("//media//kswada//MyFiles//R//ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

data(ozone, package="faraway")

str(ozone)




# ------------------------------------------------------------------------------
# Additive model by mgcv package
# bivariate transformation
# ------------------------------------------------------------------------------

library(mgcv)


# we can do bivariate transformations with mgcv.
# temperature and inverse base height (ibh) are measured on different scales and thus anisotropic.
# In this case, we will likely need a differenct amount of smoothing the two directions. This can be achieved using tensor product smooth.

amint <- mgcv::gam(O3 ~ te(temp, ibh) + s(ibt), data = ozone)

summary(amint)

anova(ammgcv, amint, test = "F")



# -->
# we see that the supposedly more complex model with the bivariate fit actually fits worse than the model with univariate funcitons,
# because fewer degrees of freedom have been used to fit the bivariate function than the two corresponding univariate functions.

# In spite of the output p-value, we suspect that there is no interaction effect,
# because the fitting algorithm is able to fit the bivariate function so simply.



# ------------------------------------------------------------------------------
# Plot contour plot and perspective 3-d plot
# ------------------------------------------------------------------------------

plot(amint, select = 1)

vis.gam(amint, theta = -45, color = "gray")



# -->
# The bivariate contour plot for temperature and ibh is shown in the left panel.
# The middle panel shows the univariate transformation on ibt while the right panel shows a perspective view of the information on the left panel.

# Given that the countours appear almost parallel and the perspective view looks like 
# it could be constructed with peice of paper rippled in one direction,
# we conclude that there is no significant interaction.
# One interesting side effect is that ibt is now significant.


