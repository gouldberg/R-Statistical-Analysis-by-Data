# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)



# ----------
Squid$fMONTH = factor(Squid$MONTH)



# ----------
M1 <- lm(Testisweight ~ DML * fMONTH, data = Squid)

mod_obj <- M1



# ------------------------------------------------------------------------------
# Pearson residual, Standardized residuals
#   - Plots the residuals versus each term in a mean function and versus fitted values
#   - Also computes a curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero
#   - For linear models, this is Tukey's test for nonadditivity when plotting against fitted values
# ------------------------------------------------------------------------------

residualPlots(mod_obj)


residualPlots(mod_obj, type = "rstandard")


# -->
# Note that the variance of stardardized residuals are large at month 9 and 10



# ----------
# only by linear predictors
residualPlots(mod_obj, terms = ~1)



# ----------
# group by against linear predictor
residualPlot(mod_obj, type = "rstandard", groups = Squid$Testisweight >= 10, linear = FALSE)



# ------------------------------------------------------------------------------
# Raw Residuals, Pearson Residuals, Pearson Residuals Scaled, Deviance Residuals
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


plot_resid(mod=mod_obj, y = "Testisweight")



