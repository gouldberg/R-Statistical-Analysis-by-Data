
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_入力出力データ\\dbbmi")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  dbbmi
#  - Data from the Fourth Dutch Growth Study, which was a cross-sectional study that measured growth and development
#    of the Dutch population between the ages 0 and 21 years.
#    The data were kindly provided by Professor Stef van Buuren.
#  - The study measured, amongst other variables, height, weight, head circumference and age.
#  - Variables:
#       - age (years) and bmi (Bodi Maxx Index)
# ------------------------------------------------------------------------------


dbbmi <- read.csv("dbbmi.txt", header = T, sep = "\t")

# data("dbbmi", package = "gamlss.data")
  

str(dbbmi)

car::some(dbbmi)




# ------------------------------------------------------------------------------
# data exploration:  overview
# ------------------------------------------------------------------------------


lattice::xyplot(bmi ~ age, data = dbbmi, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))




# ----------
graphics.off()

plot(bmi ~ age, data = dbbmi, pch = 20)
lines(smooth.spline(dbbmi$age, dbbmi$bmi, spar = 0.75), col = "blue", lwd = 2)




# ------------------------------------------------------------------------------
# Centile (Quantile) Regression:  Parametric LMS (lambda, mu, sigam) method
#   - Assume that the Y variable has a specific distribution, centile (qunatile) curves for all p (percentage)
#     can be obtained simultaneously and do not cross
# ------------------------------------------------------------------------------


library(gamlss)



# ----------
# k = 2:  the penalty to be used in the GAIC
# apply power transformation for age

m0 <- lms(bmi, age, data = dbbmi, trans.x = TRUE, k = 2)




# ----------
# centile curves

centiles(m0)


centiles.fan(m0)
points(dbbmi$age, dbbmi$bmi, pch = 20)




# ----------
# plot distribution of y for specific values of x  --> this is not avialble for R 3.6.1

library(gamlss.util)

plotSimpleGamlss(bmi, age, m0, data = dbbmi, x.val = seq(5, 20, 5), xlim = c(-3, 32), val = 5)



m0$family

m0$power




# ----------
# effective degree of freedom

edfAll(m0)




# ----------
# alternatively m0 can be fitted by



dbbmi$Tage <- (dbbmi$age) ^ (m0$power)


m0A <- gamlss(bmi ~ pb(Tage), 
              simga.formula = ~pb(Tage), 
              nu.formula = ~pb(Tage), 
              tau.formula = ~pb(Tage), 
              family = BCTo, trace = TRUE, data = dbbmi)



# ----------
# residual diagnostics

round(Q.stats(m0, xvar = dbbmi$age), 3)



# -->
# in Z2, some absolute values > 2 ...


wp(m0, xvar = dbbmi$age, n.inter = 9)




# ----------
# fitted distribution parameters

fittedPlot(m0, x = dbbmi$age)




# ------------------------------------------------------------------------------
# Centile (Quantile) Regression:  Parametric LMS (lambda, mu, sigam) method
#   - calibration
# ------------------------------------------------------------------------------

# calibration:
# when fitted model centiles differ from the sample centiles
# and it is assumed that this failure is the same for all values of the explanatory variable

# calibration() automatically adjusts the values selected for argument cent
# so that the sample percentage of cases below each centile curve is correct,
# i.e. equal to the selected cent values


cent <- c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6)

centiles(m0, cent = cent)

calibration(m0, xvar = dbbmi$age, cent = cent)


# -->
# almost similar --> no problem for the current model




# ------------------------------------------------------------------------------
# Centile (Quantile) Regression:  Parametric LMS (lambda, mu, sigam) method
#   - focus locally
# ------------------------------------------------------------------------------


centiles.split(m0, dbbmi$age)




# ------------------------------------------------------------------------------
# Centile (Quantile) Regression:  Parametric LMS (lambda, mu, sigam) method
#   - extract outliers
# ------------------------------------------------------------------------------


newage <- dbbmi$age


mat1 <- centiles.pred(m0, xname = "age", xvalues = newage, cent = c(0.4, 99.6))


head(mat1)

tail(mat1)



# ----------
# flag for outlier  --> 56 points

mat1 <- cbind(mat1, bmi = dbbmi$bmi)

mat1 <- mat1 %>% mutate(outlier = ifelse(bmi < C0.4, 1, ifelse(bmi > C99.6, 1, 0)))

mat1 %>% filter(outlier == 1)

