setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ----------
modp <- glm(articles ~ ., data = PhdPubs, family = poisson(link = "log"))



# ------------------------------------------------------------------------------
# Raw Residuals, Pearson Residuals, Pearson Residuals Scaled, Deviance REsiduals
# ------------------------------------------------------------------------------

plot_resid <- function(mod, y){
  EP <- resid(mod, type = "pearson")
  mu <- predict(mod, type = "response")
  E <- data[,y] - mu
  phi <- sum(residuals(mod, type = "pearson")^2)/df.residual(mod) 
  EP2 <- E / sqrt(phi * mu)
  op <- par(mfrow=c(2,2))
  plot(x = mu, y = E, main = "Response Residuals")
  abline(h = 0, lty=2, col="gray");  lines(lowess(mu, E), col="blue");
  plot(x = mu, y = EP, main = "Pearson Residuals")
  abline(h = 0, lty=2, col="gray");  lines(lowess(mu, EP), col="blue");
  plot(x = mu, y = EP2, main = "Pearosn Residuals Scaled")
  abline(h = 0, lty=2, col="gray");  lines(lowess(mu, EP2), col="blue");
  
  ED <- try(resid(mod, type="deviance"), silent=TRUE)
  if( class(ED) == "try-error" ){
    print(ED)
  }
  else {      
    plot(x = mu, y = ED, main = "Deviance Residuals")
    abline(h = 0, lty=2, col="gray");  lines(lowess(mu, ED), col="blue");
    par(op)
  }
}


plot_resid(mod=modp, y = "articles")



# ------------------------------------------------------------------------------
# Residual plot by car::residualPlot
# ------------------------------------------------------------------------------

library(car)


graphics.off()

par(mfrow = c(1,1))

residualPlot(modp, type = "rstandard", col.smooth = "red", id.n = 5)



# ----------
# group by
residualPlot(modp, type = "rstandard", groups = PhdPubs$article, key = FALSE, linear = FALSE, smoother = NULL)



# -->
# peculiar pattern of diagonal band of points
# These corresponds to the different discrete values of the response variable, number of articles published.




# ------------------------------------------------------------------------------
# Studentized (deletion) residuals and Standardized Pearson residuals
# ------------------------------------------------------------------------------

# studentized Residuals
stud.resid <- rstudent(modp)


# standardized Pearson residuals
stand.resid <- rstandard(modp, type = "pearson")



# ----------
par(mfrow=c(2,1))
plot(stud.resid, ylim = c(min(-3, stud.resid), max(3, stud.resid)), main = "Studentized residuals", type = "l")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Pearson residuals", type = "l")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))



# ------------------------------------------------------------------------------
# Standardized Pearson Residuals vs. explanatory variable
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(x = PhdPubs$mentor, y = stand.resid, ylim = c(-5, 5), main = "Standardized Pearson residuals")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

smooth.stand <- loess(stand.resid ~ mentor, data = PhdPubs)
ord <- order(PhdPubs$mentor)
lines(x = PhdPubs$mentor[ord], y = predict(smooth.stand)[ord], lty = "solid", col = "red")



# ----------
# residuals against each predictor to check systematic variation
# quadratic = TRUE:  computes a curvature tst for each of the plots by adding a quadratic term and testing the quadratic to be zero
car::residualPlot(modp, "mentor", type = "rstandard", quadratic = TRUE, col.smooth = "blue", col.quad = "green", id.n = 5, ylim = c(-5, 5))



# ------------------------------------------------------------------------------
# Studentized (deletion) residuals vs. explanatory variable
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(x = PhdPubs$mentor, y = stud.resid, ylim = c(-5, 5), main = "Studentized residuals")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

smooth.stud <- loess(stud.resid ~ mentor, data = PhdPubs)
ord <- order(PhdPubs$mentor)
lines(x = PhdPubs$mentor[ord], y = predict(smooth.stud)[ord], lty = "solid", col = "red")



# ----------
# residuals against each predictor to check systematic variation
# quadratic = TRUE:  computes a curvature tst for each of the plots by adding a quadratic term and testing the quadratic to be zero
car::residualPlot(modp, "mentor", type = "rstudent", quadratic = TRUE, col.smooth = "blue", col.quad = "green", id.n = 5, ylim = c(-5, 5))


# -->
# curvature is not pronounced in blue curve (smoothed curve) such as green.


car::residualPlot(modp, "phdprestige", type = "rstudent", quadratic = TRUE, col.smooth = "blue", col.quad = "green", id.n = 5, ylim = c(-5, 5))



