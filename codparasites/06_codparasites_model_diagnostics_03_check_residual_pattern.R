setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ----------
modp <- glm(intensity ~ length + area * year, data = CodParasites, family = poisson(link = "log"))



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


plot_resid(mod=modp, y = "intensity")



# ------------------------------------------------------------------------------
# Residual plot by car::residualPlot
# ------------------------------------------------------------------------------

library(car)


graphics.off()

par(mfrow = c(1,1))

residualPlot(modp, type = "rstandard", col.smooth = "red", id.n = 5)



# ----------
# group by
residualPlot(modp, type = "rstandard", groups = CodParasites$area, linear = FALSE, col = c("black","red", "skyblue", "darkgreen"))



# -->
# Note that varangerfjord has negative Rstandard residuals at upper end



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

# Note that here na.omit is required

par(mfrow=c(1,1))
plot(x = na.omit(CodParasites)$length, y = stand.resid, ylim = c(-5, 5), main = "Standardized Pearson residuals")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

smooth.stand <- loess(stand.resid ~ length, data = na.omit(CodParasites))
ord <- order(na.omit(CodParasites)$length)
lines(x = na.omit(CodParasites)$length[ord], y = predict(smooth.stand)[ord], lty = "solid", col = "red")



# ----------
# residuals against each predictor to check systematic variation
# quadratic = TRUE:  computes a curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero
car::residualPlot(modp, "length", type = "rstandard", quadratic = TRUE, col.smooth = "blue", col.quad = "green", id.n = 5, ylim = c(-5, 5))



# ------------------------------------------------------------------------------
# Studentized (deletion) residuals vs. explanatory variable
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(x = na.omit(CodParasites)$length, y = stud.resid, ylim = c(-5, 5), main = "Studentized residuals")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

smooth.stud <- loess(stud.resid ~ length, data = na.omit(CodParasites))
ord <- order(na.omit(CodParasites)$length)
lines(x = na.omit(CodParasites)$length[ord], y = predict(smooth.stud)[ord], lty = "solid", col = "red")



# ----------
# residuals against each predictor to check systematic variation
# quadratic = TRUE:  computes a curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero
car::residualPlot(modp, "length", type = "rstudent", quadratic = TRUE, col.smooth = "blue", col.quad = "green", id.n = 5, ylim = c(-5, 5))


# -->
# curvature is NOT pronounced in blue curve (smoothed curve) such as green.




