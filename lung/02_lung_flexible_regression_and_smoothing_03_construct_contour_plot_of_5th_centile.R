setwd("//media//kswada//MyFiles//R//lung")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lung
# ------------------------------------------------------------------------------

lung <- read.csv("data.csv", header = T)

str(lung)

car::some(lung)


# ----------
# only males data

dm <- subset(lung, sex == 1)

dim(dm)



# ----------
# apply a log transformation to height and age

dm <- transform(dm, la = log(age), lh = log(height))



# ------------------------------------------------------------------------------
# Fit a model for height against age to find lower and upper centile limits of height for each age
# ------------------------------------------------------------------------------

k1 <- log(nrow(dm))


mh <- gamlss(height ~ pb(log(age), method = "GAIC", k = k1),
             sigma.fo = ~pb(log(age), method = "GAIC", k = k1),
             nu.fo = ~pb(log(age), method = "GAIC", k = k1),
             tau.fo = ~pb(log(age), method = "GAIC", k = k1),
             family = BCTo, data = dm)


mh

term.plot(mh)

term.plot(mh, what = "sigma")



# ------------------------------------------------------------------------------
# Plot the centiles for height against age
# ------------------------------------------------------------------------------

centiles(mh, xvar = dm$age, cent = c(0.1, 0.4, 2, 10, 25, 50, 75, 90, 98, 99.6, 99.9), ylab = "height", xlab = "age", legend = FALSE)



# ------------------------------------------------------------------------------
# Find lower (0.1%) and upper (99.9%) limits for height given age
# ------------------------------------------------------------------------------

newage <- seq(5, 90, 0.1)

newcent <- c(0.1, 50, 99.9)

maty <- centiles.pred(mh, xname = "age", xvalues = newage, cent = newcent, plot = TRUE)

maty[1:10,]



# ------------------------------------------------------------------------------
# Construct a contour plot of the 5th centile of fev against height and age
# ------------------------------------------------------------------------------

# Expand a grid of values of age from 5 to 90 years and height from 100 to 210 cm to cover the limits of height
newdata <- expand.grid(age = seq(5, 90, 0.1), height = seq(100, 210, 1))


# Use the chosen model m3 for fev to predict all the parameters mu, sigma, nu and tau of the distribution BCTo for the values of age and height in newdata.
m3p <- predictAll(m3, newdata = newdata, type = "response")


m3p


# Calculate the 5th centile of fev for all cases in newdata
fev5 <- qBCPE(0.05, m3p$mu, m3p$sigma, m3p$nu, m3p$tau)



# ----------
# For all cases of newdata with values of height outside the lower (0.1%) and upper (99.9%) bounds for height,
# replace the value of fev5 with a missing value (NaN)

lower <- rep(maty[,2], 111)

upper <- rep(maty[,4], 111)

fev5a <- ifelse(((newdata$height < lower) | (newdata$height > upper)), NaN, fev5)



# ----------
# Obtain a contour plot of the 5th centile of fev against height and age.
newheight <- seq(100, 210, 1)

newage <- seq(5, 90, 0.1)

mfev5 <- matrix(data = fev5a, nrow = 851, ncol = 111)

contour(newage, newheight, mfev5, nlevels = 40, xlab = "age(years)", ylab = "height(cm)")


