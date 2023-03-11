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




# ------------------------------------------------------------------------------
# default diagnostics plot
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,2))

plot(lmod)




# ------------------------------------------------------------------------------
# residual plot:  residual vs. predictors, fitted
# ------------------------------------------------------------------------------


graphics.off()


par(mfrow = c(2,3))

var_obj <- names(coef(lmod))[-1]

lapply(var_obj, function(x){
  plot(gala[,x], residuals(lmod), main = paste0(x))
  abline(h = 0, lty = 2, col = "black")
})


plot(fitted(lmod), residuals(lmod), main = "fitted")
abline(h = 0, lty = 2, col = "black")





# ------------------------------------------------------------------------------
# normality of residuals
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(1,1))

qqnorm(residuals(lmod))

qqline(residuals(lmod))



# ----------
hist(residuals(lmod), breaks = seq(-180, 220, by = 20))



# ----------
# Null Hypothesis:  the residuals are normal
shapiro.test(residuals(lmod))



# -->
# rejecting




# ------------------------------------------------------------------------------
# residual plot by car package
# ------------------------------------------------------------------------------

car::residualPlots(lmod, type = "response")


