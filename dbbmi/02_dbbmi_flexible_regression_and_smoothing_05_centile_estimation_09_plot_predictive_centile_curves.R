setwd("//media//kswada//MyFiles//R//dbbmi")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dbbmi
# ------------------------------------------------------------------------------
data("dbbmi", package = "gamlss.data")


str(dbbmi)

car::some(dbbmi)



# ------------------------------------------------------------------------------
# Case1:  Centile for y given x and centile percentage
# ------------------------------------------------------------------------------

m0 <- lms(bmi, age, data = dbbmi1, trans.x = TRUE, k = 2)

dbbmi1$Tage <- (dbbmi1$age) ^ (m0$power)

m0A <- gamlss(bmi ~ pb(Tage), sigma.fo = ~pb(Tage), nu.fo = ~pb(Tage), family = BCCGo, trace = FALSE, data = dbbmi1)




# ----------
newage <- seq(0.05, 2, 0.05)


# model m0 by LMS method
mat1 <- centiles.pred(m0, xname = "age", xvalues = newage, cent = c(5, 25, 50, 75, 95), plot=TRUE, legend = FALSE, xlab = "age", ylab = "BMI")


head(mat1)



# ----------
# model m0A by gamlss method
mat1_2 <- centiles.pred(m0A, xname = "Tage", xvalues = newage, cent = c(5, 25, 50, 75, 95), plot=TRUE, legend = FALSE, xlab = "transfomed age", ylab = "BMI")


head(mat1_2)



# -->
# THe results are different for age close to zero,
# because prediction for a gamlss model involves some refitting, while for an lms model prediction involves
# interpolating or extrapolating the fitted spline functions,
# which may be preferable.

# Note also that including newage = 0 in the prediction range may lead to an unreliable prediction at age = 0
# because its power transformed age is well outside the power transformed age range of the original data.

# For model m0A, this also leads to slightly changed predictions
# within the origianl age range, because the default 20 P-spline knots are spread over an expanded transformed age range,
# resulting in fewer knots within the transformed original age range.



# ------------------------------------------------------------------------------
# Case2:  Centile for y given x and centile z-score
# ------------------------------------------------------------------------------

dev <- c(-4, -3, -2, -1, 0, 1, 2, 3, 4)

round(100 * dNO(dev), 3)


mat2 <- centiles.pred(m0, xname = "age", xvalues = newage, type = "standard-centiles", dev = dev, plot = T)

head(mat2)


