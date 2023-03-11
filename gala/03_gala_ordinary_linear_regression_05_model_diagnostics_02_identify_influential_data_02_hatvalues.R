]setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)




# ----------
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)




# ------------------------------------------------------------------------------
# Identify influential observations:  hat values (leverage)
# ------------------------------------------------------------------------------


hatvalues(lmod)




# ----------
x <- model.matrix(~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)

y <- gala$Species



# squared Mahalanobis distance in x-space  (only defined by predictors)
# here, x-space is 5 dimensions
H <- x %*% solve(t(x) %*% x) %*% t(x)


( hat <- diag(H) )


hatvalues(lmod)




# ----------
# y_hat = H %*% y

H %*% y


lmod$fitted.values




# ----------
# residual = y - y_hat = y - H %*% y
# large "hat" make residuals small  (--> "leverage")

c(y - H %*% y)


lmod$residuals



# for Baltra

y[1]

H[1,] %*% y

y[1] - H[1,] %*% y

lmod$residuals[1]




# ----------
# sum of hatvalues = # of predictors + 1 (intercept)

sum(hatvalues(lmod))

length(coef(lmod))




# ----------

sort(round(hatvalues(lmod), 3), decreasing = T)


( crit <- c(2, 3) * sum(hatvalues(lmod)) / nrow(gala) )


hatvalues(lmod)[hatvalues(lmod) >= crit[1]]

hatvalues(lmod)[hatvalues(lmod) >= crit[2]]


