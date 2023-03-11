setwd("//media//kswada//MyFiles//R//texaselectr")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Texas Electr
# ------------------------------------------------------------------------------

data("TexasElectr", package = "pder")


str(TexasElectr)


dim(TexasElectr)


car::some(TexasElectr)



# ------------------------------------------------------------------------------
# Construct matrix for restrictions by coef
# ------------------------------------------------------------------------------

# Factor shares being the derivative of the cost function, the following restrictions must be imposed:
# 1. the coef of pl in the cost equation must equal the intercept in the labor share equation
# 2. the coef of pk in the cost equation must equal the intercept in the capital share equation
# 3. the coef of pll in the cost equation should equal the coef associated to pl in the labor share equation
# 4. the coef of pkk in the cost equation should be equal to the coef associated to pk in the capital share equation
# 5. the coef of plk in the cost equation should equal the coef of pk in the labor share equation and the coef of pl in the capita lshare equation

# We construct for this purpose a 6 (number of restrictions) by 14 (number of coefficients) matrix
# The first lne of the matrix indicates that the second coef (the one associated to pl in the cost equation) must be equal to the ninth
# (the constant term in the labor share equation)

R <- matrix(0, nrow = 6, ncol = 14)

R[1, 2] <- R[2, 3] <- R[3, 5] <- R[4, 6] <- R[5, 6] <- R[6, 7] <- 1

R[1, 9] <- R[2, 12] <- R[3, 10] <- R[4, 11] <- R[5, 13] <- R[6, 14] <- -1



# ------------------------------------------------------------------------------
# Estimate Seemingly Unrelated Regression (SUR) model
# ------------------------------------------------------------------------------


# model = "random":  in order to estimate the SUR error components model
# If, as happens here, all elements of q are zero, the restrict.rhs argument can be omitted.

z <- plm(list(cost = C ~ pl + pk + q + pll + plk + pkk + qq,
              shlab = sl ~ pl + pk,
              shcap = sk ~ pl + pk),
         TexasElectr, model = "random",
         restrict.matrix = R)


summary(z)



# -->
# The results indicate the presence of increasing returns to scale, as q (=0.85) is significantly lower than 1.
# Factor shares at the sample mean for labor and capital are respectively 12 and 31%

