# ------------------------------------------------------------------------------
# Data:  RoadKills (Amphibian Roadkills)
#
# Ecological Question:  is there a relationship between amphibian roadkills and any of the explanatory variables ?
# 
# The data set consists of roadkills of amphibian species at 52 sites along a road in Portugal
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "vcdExtra", "effects", "MASS", "VGAM", "car", "broom", "psych")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.table(file = "./ZuurDataMixedModelling/RoadKills.txt", header=T, dec=".")
dim(data)
str(data)

Hmisc::describe(data)


# ------------------------------------------------------------------------------
# Investigate the distribution of response variable (count data)
# ------------------------------------------------------------------------------
library(vcd)

# Poisson
rootogram(goodfit(data$TOT.N), xlab="road killings")

# compare with negative binomial  --> NO ZERO
rootogram(goodfit(data$TOT.N, type="nbinomial"), xlab="road killings", main="Negative binomial")


resp.fac <- factor(data$TOT.N, levels = min(data$TOT.N):max(data$TOT.N))
( mu <- mean(data$TOT.N) )
( ci <- mu + c(-1, 1) * sd(data$TOT.N) )

barplot(table(resp.fac), xlab = "road killings", ylab = "Frequency", col = "lightblue")
abline(v = mu, col = "red", lwd = 3)
lines(x = ci, y = c(-4, -4), col = "red", lwd = 3, xpd = TRUE)


# similar in log scale
barplot(table(resp.fac)+1, xlab = "road killings", ylab = "log(Frequency+1)", col = "lightblue", log = "y")
abline(v = mu, col = "red", lwd = 3)
lines(x = ci, y = c(0.9, 0.9), col = "red", lwd = 3, xpd = TRUE)


# ------------------------------------------------------------------------------
# Check for spatial data
# The data were measured along the road, and the sampling positions are marked as dots
# ------------------------------------------------------------------------------
# plot in isometric scales, where the relation between physical distance on the device and distance in the data scale are forced to be the same for both axes
xyplot(Y ~ X, cl=1, pch=16, data = data)
xyplot(Y ~ X, aspect = "iso", cl=1, pch=16, data = data)


# Just Poisson Regression
mod <- glm(TOT.N ~ D.PARK, family = poisson, data = data)
summary(mod)

tmp <- data.frame(D.PARK = seq(0, 25000, by = 1000))
pred <- predict(mod, newdata = tmp, type = "link", se=TRUE)
fit <- exp(pred$fit);  upper <- exp(pred$fit + 1.96 * pred$se.fit);  lower <- exp(pred$fit - 1.96 * pred$se.fit)
plot(TOT.N ~ D.PARK, data = data)
lines(tmp$D.PARK, fit, lty = 1)
lines(tmp$D.PARK, upper, lty = 2, col="gray")
lines(tmp$D.PARK, lower, lty = 2, col="gray")


# ------------------------------------------------------------------------------
# Investigate the relationship among variables
# ------------------------------------------------------------------------------
var <- colnames(data);  var <- setdiff(var, c("X", "Y", "Sector"))


# marical and conditional plots
gpairs(data[,var],
       diag.pars = list(fontsize = 16, hist.color ="lightgray",
                        mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate=1:4)),
                        outer.rot = c(45,45)))


# check correlation
psych::pairs.panels(data[, var])


# corrplot
# correlation is very small 
par(mfrow=c(1,1))
corrplot(cor(data[, colnames(data) %in% var])) 


# ------------------------------------------------------------------------------
# Investigate Box-Cox Transformation possibility
# ------------------------------------------------------------------------------
source("./utilities/BoxCoxTransInvestigate.R")

var <- colnames(data);  var <- setdiff(var, c("X", "Y", "Sector", "TOT.N"))
BoxCoxTrans_byvar(data = data[,var], var = var)

hist(data[,"POLIC"])


# ------------------------------------------------------------------------------
# Transformation
# ------------------------------------------------------------------------------
data$SQ.POLIC <- sqrt(data$POLIC)
data$SQ.WATRES <- sqrt(data$WAT.RES)
data$SQ.URBAN <- sqrt(data$URBAN)
data$SQ.OLIVE <- sqrt(data$OLIVE)
data$SQ.LPROAD <- sqrt(data$L.P.ROAD)
data$SQ.SHRUB <- sqrt(data$SHRUB)
data$SQ.DWATCOUR <- sqrt(data$D.WAT.COUR)


# ------------------------------------------------------------------------------
# Investigate Variance Inflation Factor
# and select variables by VIF's criteria
# ------------------------------------------------------------------------------
source("./utilities/CorVif.R")

var <- c("OPEN.L", "SQ.OLIVE", "MONT.S", "MONT", "SQ.POLIC", "SQ.SHRUB", "SQ.URBAN", "SQ.WATRES", "L.WAT.C", "L.D.ROAD", "SQ.LPROAD", "D.WAT.RES", "SQ.DWATCOUR", "D.PARK", "N.PATCH", "P.EDGE", "L.SDI")
corvif(data = data[,var], var = var)
# car::vif(data[,var])

crit <- 3
var_slcbycorvif <- select_var_bycorvif(data = data[,var], var = var, crit = crit)

var_slc <- rownames(var_slcbycorvif)


# ------------------------------------------------------------------------------
# Scatterplot by each selected variable against response variable
# ------------------------------------------------------------------------------
# X: explanatory variables  Y: TOT.N (killings)
tmp <- unlist(data[,var_slc])
killings <- rep(data$TOT.N, length(var_slc))
id <- rep(rep(var_slc, each = nrow(data)), length(var_slc))


# Check linear patterns --> did not show any clear linear patterns --> using a GAM
xyplot(killings ~ tmp | id,
       col = 1,
       strip = function(bg = "white", ...) strip.default(bg = "white", ...),
       scales = list(alternating = TRUE, x = list(relation = "free"), y = list(relation = "same")),
      xlab = "Explanatory variables", ylab = "Number of roadkillings",
      panel = function(x, y){
        panel.grid(h = 01, v = 2)
        panel.points(x, y, col = 1)
        panel.loess(x, y, col = "blue", lwd = 2)
      }
)


# ------------------------------------------------------------------------------
# Get a feel for the spatial patterns of the explanatory variables
# and drop variables with clear pattern with spatial variables (D.PARK)
# ------------------------------------------------------------------------------
# X: D.PARK  Y: explanatory variables
var <- setdiff(var_slc, "D.PARK")
tmp <- unlist(data[,var])
d.park <- rep(data$D.PARK, length(var))
id <- rep(rep(var, each = nrow(data)), length(var))

# SQ.OLIVE, D.WAT.RES, and L.D.ROAD show a clear pattern with D.PARK
xyplot(tmp ~ d.park | id,
       col = 1,
       strip = function(bg = "white", ...) strip.default(bg = "white", ...),
       scales = list(alternating = TRUE, x = list(relation = "same"), y = list(relation = "free")),
       xlab = "D.PARK", ylab = "Explanatory Variables",
       panel = function(x, y){
         panel.grid(h = 01, v = 2)
         panel.points(x, y, col = 1)
         panel.loess(x, y, col = "blue", lwd = 2)
       }
)


# ------------------------------------------------------------------------------
# Poisson regressions
# ------------------------------------------------------------------------------
source("./utilities/Drop1PoisReg.R")

y_var <- "TOT.N"

var_trans_slc <- rownames(var_slcbycorvif)
var_trans_slc_exc <- setdiff(var_trans_slc, c("SQ.OLIVE", "D.WAT.RES", "L.D.ROAD"))

char1 <- paste0(var_trans_slc, collapse = " + ")
char2 <- paste0(var_trans_slc_exc, collapse = " + ")

# simple poisson and stepAIC
eval(parse(text = paste0("mod.pois <- glm(", y_var, " ~ ", char1, ", family = poisson, data = data)")))
mod.pois.aic <- stepAIC(mod.pois, direction="both", trace=TRUE)
varn <- length(mod.pois.aic$coefficients)
char0 <- paste0(names(mod.pois.aic$coefficients[2:varn]), collapse = " + ")
eval(parse(text = paste0("mod.pois.aic <- glm(", y_var, " ~ ", char0, ", family = poisson, data = data)")))

# quassipoisson and select by repeated drop1
eval(parse(text = paste0("mod.qpois <- glm(", y_var, " ~ ", char1, ", family = quasipoisson, data = data)")))

crit <- 0.05
mod.qpois.slc <- drop1_byvar_qpois(data = data, x_var = var_trans_slc, y_var = y_var, crit = crit)


# negative binomial and select by repeated drop1
eval(parse(text = paste0("mod.nbin <- glm.nb(", y_var, " ~ ", char1, ", data = data)")))
theta <- mod.nbin$theta
eval(parse(text = paste0("mod.nbin <- glm(", y_var, " ~ ", char1, ", data = data, family=negative.binomial(theta))")))

crit <- 0.05
mod.nbin.slc <- drop1_byvar_nbin(data = data, x_var = var_trans_slc, y_var = y_var, crit = crit)


# Model Comparison
source("./utilities/ModelComparisonPoisReg.R")
mod_name <- c("mod.pois", "mod.pois.aic", "mod.qpois", "mod.qpois.slc", "mod.nbin", "mod.nbin.slc")
mod_list <- list(mod.pois, mod.pois.aic, mod.qpois, mod.qpois.slc, mod.nbin, mod.nbin.slc)

( modcomp <- ModCompPoisReg(mod_list = mod_list, mod_name = mod_name) )


# plot residuals
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

plot_resid(mod = mod.pois, y = y_var)
plot_resid(mod = mod.nbin.slc, y = y_var)


# ------------------------------------------------------------------------------
# GAM
# ------------------------------------------------------------------------------
source("./utilities/Drop1PoisReg.R")

library(mgcv);  library(MASS);

y_var <- "TOT.N"

# model is applying cross-validation. some combinations of smoothers will use more than 52 df.
mod.gam <- gam(TOT.N ~ s(OPEN.L) + s(MONT.S) + s(SQ.POLIC) + s(SQ.SHRUB) + s(SQ.WATRES) + s(L.WAT.C) + s(SQ.LPROAD) + s(SQ.DWATCOUR) + s(D.PARK), 
               family = negative.binomial(1), data = data)

# extend the model to s(OPEN.L, k=4)
var_trans_slc_exc_gam <- setdiff(var_trans_slc, c("SQ.OLIVE", "D.WAT.RES", "L.D.ROAD"))
var_trans_slc_exc_gam <- paste0("s(", var_trans_slc_exc_gam, ", k=k)")
char <- paste0(var_trans_slc_exc_gam, collapse = " + ")

theta <- 1
k <- 4
eval(parse(text = paste0("mod.gam <- gam(", y_var, " ~ ", char, ", family = negbin(theta), data = data)")))


theta <- 1
k <- 4
mod.gam.slc <- gam(TOT.N ~ s(OPEN.L, k = k, bs = "ts") + s(D.PARK, k = k, bs="ts"), family=negbin(theta), data = data)
mod.gam.slc <- gam(TOT.N ~ s(OPEN.L) + s(D.PARK), family=negbin(theta), data = data)
summary(mod.gam.slc)
anova(mod.gam.slc)

plot(mod.gam.slc)



mod <- gam(TOT.N ~ s(D.PARK), data = data, family = negbin(1))
pred <- predict(mod, se=TRUE, type="response")
par(mfrow=c(1,1))
plot(data$D.PARK, data$TOT.N, cex = 1.1, pch = 16, main = "Nagative Binomial GAM", xlab = "Distance to Park", ylab = "Number of road killings", col="blue")
ord <- order(data$D.PARK)
lines(data$D.PARK[ord], pred$fit[ord], lwd = 2)
lines(data$D.PARK[ord], pred$fit[ord] + 1.96 * pred$se.fit[ord], lty = 2, lwd = 2, col="gray")
lines(data$D.PARK[ord], pred$fit[ord] - 1.96 * pred$se.fit[ord], lty = 2, lwd = 2, col="gray")
for(i in 1:nrow(data)){
  y <- rnbinom(100, size = 11.8, mu = pred$fit[i])
  points(rep(data$D.PARK[i], 100), y, cex = 0.5)
}

plot(mod, residuals=TRUE, se=TRUE, pch=".")

