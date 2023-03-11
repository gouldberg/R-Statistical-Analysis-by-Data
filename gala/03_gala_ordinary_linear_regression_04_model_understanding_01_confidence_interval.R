setwd("//media//kswada//MyFiles//R//gala")

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
# Ordinary Linear Regression
# ------------------------------------------------------------------------------

# Not include Endemics
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)


summary(lmod)



# ------------------------------------------------------------------------------
# Confidence interval for coefficient
# ------------------------------------------------------------------------------

df <- 24


# 97.5% point of t distribution (df = 24)
qt(0.975, df)


coef(lmod) + t(c(-1, 1) %*% t( qt(0.975, df) * lmodsum$coef[,"Std. Error"] ))


confint(lmod)




# ----------
# joint confidence interval region

library(ellipse)

par(mfrow = c(1,1))


# Pearson Correlation = 0.8
# 95% region
plot(ellipse(0.8), type = 'l')



# ----------
plot(ellipse(lmod, c("Area","Adjacent")), type = "l", ylim = c(-0.13, 0))

points(coef(lmod)["Area"], coef(lmod)["Adjacent"], pch = 19)

abline(v = confint(lmod)["Area", ], lty = 2)

abline(h = confint(lmod)["Adjacent", ], lty = 2)




# -->
# Null Hypothesis: both coeffs of Area and Adjacent = 0 is rejected, outside of the ellipse
# Coef of Area = 0 is not rejected because zero is within.




# ------------------------------------------------------------------------------
# Bootstrap Confidence Interval
# ------------------------------------------------------------------------------

set.seed(123)


# 4000 times bootstrapping
nb <- 4000

coefmat <- matrix(NA, nb, 6)

resids <- residuals(lmod)


preds <- fitted(lmod)


for(i in 1:nb){
  booty <- preds + sample(resids, rep = TRUE)
  
  bmod <- update(lmod, booty ~ .)
  
  coefmat[i,] <- coef(bmod)
}



colnames(coefmat) <- c("Intercept", colnames(gala[,3:7]))


coefmat <- data.frame(coefmat)


head(coefmat)


( tmp <- apply(coefmat, 2, function(x) quantile(x, c(0.025, 0.975))) )



# ----------

library(ggplot2)


ggplot(coefmat, aes(x = Area)) + geom_density() + geom_vline(xintercept = tmp[,"Area"], lty = 2) + theme_bw()


ggplot(coefmat, aes(x = Adjacent)) + geom_density() + geom_vline(xintercept = tmp[,"Adjacent"], lty = 2) + theme_bw()




# ------------------------------------------------------------------------------
# prediction
# ------------------------------------------------------------------------------

predict(lmod)



# ----------
( mod_mat <- model.matrix(lmod) )


( pred <- coef(lmod) %*% t(mod_mat) )




# ------------------------------------------------------------------------------
# confidence interval for Y
# ------------------------------------------------------------------------------

predict(lmod, interval = "confidence")




# ------------------------------------------------------------------------------
# confidence interval and prediction interval for data
# ------------------------------------------------------------------------------


# extreme 95% point data
( x1 <- apply(mod_mat, 2, function(x) quantile(x, 0.95)) )



predict(lmod, new = data.frame(t(x1)), interval = "confidence")

predict(lmod, new = data.frame(t(x1)), interval = "prediction")



# -->
# prediction interval is wider than confidence interval

