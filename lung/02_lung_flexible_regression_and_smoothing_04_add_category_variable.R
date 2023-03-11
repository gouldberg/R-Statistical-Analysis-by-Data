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
# inlucde "centre" (four levels category)
# ------------------------------------------------------------------------------
# Use a local SBC to choose the effective degrees of freedom for smoothing terms
# The reason for using SBC is to achieve smooth centiles.
# A lower value of k (e.g. k = 4) would result in less smooth centiles but a better fit to the data,
# while a higher value of k would result in even smoother centiles, but a worse fit to the data.

table(dm$centre)


k1 <- log(nrow(dm))


# m6 <- stepGAICAll.A(m1, scope = list(lower = ~1, 
#                      upper = ~pb(lh, medhot = "GAIC", k = k1) + pb(la, method = "GAIC", k = k1) + 
#                      pvc(lh, by = as.factor(centre), method = "GAIC", k = k1) + 
#                      pvc(la, by = as.factor(centre), method = "GAIC", k = k1)), k = k1)

m6 <- stepGAICAll.A(m1, scope = list(lower = ~1, 
                                     upper = ~pb(lh, medhot = "GAIC", k = k1) + pb(la, method = "GAIC", k = k1) + as.factor(centre)), k = k1)


# ----------
summary(m6)



# ------------------------------------------------------------------------------
# Refit the chosen model, but replacing lh and la by log(height) and log(age)
# in order to use predictALl afterwards
# ------------------------------------------------------------------------------

m7 <- gamlss(fev ~ pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1) + as.factor(centre), 
             sigma.fo = ~pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1) + as.factor(centre),
            nu.for = ~1, tau.fo = ~1, family = BCTo, data = dm, n.cyc = 100)


summary(m7)



# ------------------------------------------------------------------------------
# Amend mode m7 to fit distribution BCCGo and BCPEo
# ------------------------------------------------------------------------------

m8 <- gamlss(fev ~ pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1) + as.factor(centre), 
             sigma.fo = ~pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1) + as.factor(centre),
             nu.for = ~1, tau.fo = ~1, family = BCCGo, data = dm, n.cyc = 100)



m9 <- gamlss(fev ~ pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1) + as.factor(centre), 
             sigma.fo = ~pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1) + as.factor(centre),
             nu.for = ~1, tau.fo = ~1, family = BCPEo, data = dm, n.cyc = 100)


GAIC(m3, m4, m5, m7, m8, m9, k = log(nrow(dm)))



# -->
# m7 has the lowest SBC



# ------------------------------------------------------------------------------
# Check the adequacy of model m7 using residual diagnostics
# ------------------------------------------------------------------------------

plot(m7)



# ----------
wp(m7, ylim.all = 0.6)

wp(m7, xvar = ~age + height + centre, n.inter = 4, ylim.worm = 1)




# ------------------------------------------------------------------------------
# Effective degrees of freedom (including 2 for the constant and linear terms)
# ------------------------------------------------------------------------------

edfAll(m7)



# ------------------------------------------------------------------------------
# Fitted smooth functions
# ------------------------------------------------------------------------------

term.plot(m7, what = "mu", pages = 1, ask = FALSE)

term.plot(m7, what = "sigma", pages = 1, ask = FALSE)



# ------------------------------------------------------------------------------
# Fit a model for height against age to find lower and upper centile limits of height for each age, for only centre == 2
# ------------------------------------------------------------------------------

dm2 <- subset(dm, centre == 2)


k1 <- log(nrow(dm2))


mh2 <- gamlss(height ~ pb(log(age), method = "GAIC", k = k1),
             sigma.fo = ~pb(log(age), method = "GAIC", k = k1),
             nu.fo = ~pb(log(age), method = "GAIC", k = k1),
             tau.fo = ~pb(log(age), method = "GAIC", k = k1),
             family = BCTo, data = dm2)


mh2

term.plot(mh2)

term.plot(mh2, what = "sigma")



# ------------------------------------------------------------------------------
# Plot the centiles for height against age
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
centiles(mh2, xvar = dm2$age, cent = c(0.1, 0.4, 2, 10, 25, 50, 75, 90, 98, 99.6, 99.9), ylab = "height", xlab = "age", legend = FALSE)



# ------------------------------------------------------------------------------
# Find lower (0.1%) and upper (99.9%) limits for height given age
# ------------------------------------------------------------------------------

newage <- seq(5, 90, 0.1)

newcent <- c(0.1, 50, 99.9)

maty2 <- centiles.pred(mh2, xname = "age", xvalues = newage, cent = newcent, plot = TRUE)

maty2[1:10,]



# ------------------------------------------------------------------------------
# Construct a contour plot of the 5th centile of fev against height and age
# ------------------------------------------------------------------------------

# Expand a grid of values of age from 5 to 90 years and height from 100 to 210 cm to cover the limits of height
newdata <- expand.grid(age = seq(5, 90, 0.1), height = seq(100, 210, 1), centre = c(2))


# Use the chosen model m7 for fev to predict all the parameters mu, sigma, nu and tau of the distribution BCTo for the values of age and height in newdata.
m7p <- predictAll(m7, newdata = newdata, type = "response")


m7p


# Calculate the 5th centile of fev for all cases in newdata
fev52 <- qBCPE(0.05, m7p$mu, m7p$sigma, m7p$nu, m7p$tau)



# ----------
# For all cases of newdata with values of height outside the lower (0.1%) and upper (99.9%) bounds for height,
# replace the value of fev5 with a missing value (NaN)

lower <- rep(maty[,2], 111)

upper <- rep(maty[,4], 111)

fev5a2 <- ifelse(((newdata$height < lower) | (newdata$height > upper)), NaN, fev52)



# ----------
# Obtain a contour plot of the 5th centile of fev against height and age.
newheight <- seq(100, 210, 1)

newage <- seq(5, 90, 0.1)

mfev52 <- matrix(data = fev5a2, nrow = 851, ncol = 111)



# not including "centre" and including "centre" but only centre 2
par(mfrow=c(1,2))
contour(newage, newheight, mfev5, nlevels = 40, xlab = "age(years)", ylab = "height(cm)")
contour(newage, newheight, mfev52, nlevels = 40, xlab = "age(years)", ylab = "height(cm)")


