# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ----------
M1 <- glm(TotAbund ~ MeanDepth, data = DF, family = poisson(link = "log"))

M2 <- glm(TotAbund ~ MeanDepth * fPeriod,  data = DF,  family = poisson)

DF$LogSA <- log(DF$SweptArea)

M3 <- glm(TotAbund ~ MeanDepth * factor(Period) + offset(LogSA), data = DF,  family = poisson)



# ----------
mod_obj <- M3



# ------------------------------------------------------------------------------
# Pearson residual, Standardized residuals
#   - Plots the residuals versus each term in a mean function and versus fitted values
#   - Also computes a curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero
#   - For linear models, this is Tukey's test for nonadditivity when plotting against fitted values
# ------------------------------------------------------------------------------

residualPlots(mod_obj)


residualPlots(mod_obj, type = "rstandard")



# ----------
# only by linear predictors
residualPlots(mod_obj, terms = ~1)



# ----------
# group by against linear predictor
residualPlot(mod_obj, type = "rstandard", groups = DF$fPeriod, linear = FALSE)



# ------------------------------------------------------------------------------
# Raw Residuals, Pearson Residuals, Pearson Residuals Scaled, Deviance Residuals
# ------------------------------------------------------------------------------

plot_resid <- function(data, mod, y){
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


plot_resid(data = DF, mod=mod_obj, y = "TotAbund")



