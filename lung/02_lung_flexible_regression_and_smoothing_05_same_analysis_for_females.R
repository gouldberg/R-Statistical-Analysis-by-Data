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
# only females data

dfe <- subset(lung, sex == 2)

dim(dfe)



# ----------
# apply a log transformation to height and age

dfe <- transform(df, la = log(age), lh = log(height))



# ------------------------------------------------------------------------------
# Search for a suitable model for fev using BCTo (Box-Cox t original) distribution by stepGAICAll.A
# ------------------------------------------------------------------------------

# Starting from a model m1 with constant parameters
m1_fe <- gamlss(fev ~ 1, sigma.fo = ~1, nu.fo = ~1, tau.fo = ~1, family = BCTo, data = dfe, n.cyc = 100)



# ----------
# Use a local SBC to choose the effective degrees of freedom for smoothing terms
# The reason for using SBC is to achieve smooth centiles.
# A lower value of k (e.g. k = 4) would result in less smooth centiles but a better fit to the data,
# while a higher value of k would result in even smoother centiles, but a worse fit to the data.

k1 <- log(nrow(dfe))

m2_fe <- stepGAICAll.A(m1_fe, scope = list(lower = ~1, upper = ~pb(lh, medhot = "GAIC", k = k1) + pb(la, method = "GAIC", k = k1)), k = k1)



# ----------
summary(m2_fe)



# ------------------------------------------------------------------------------
# Refit the chosen model, but replacing lh and la by log(height) and log(age)
# in order to use predictALl afterwards
# ------------------------------------------------------------------------------

m3_fe <- gamlss(fev ~ pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             sigma.fo = ~pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             nu.for = ~1, tau.fo = ~1, family = BCTo, data = dfe, n.cyc = 100)


summary(m3_fe)



# ------------------------------------------------------------------------------
# Amend mode m3_fe to fit distribution BCCGo and BCPEo
# ------------------------------------------------------------------------------

m4_fe <- gamlss(fev ~ pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             sigma.fo = ~pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             nu.for = ~1, tau.fo = ~1, family = BCCGo, data = dfe, n.cyc = 100)



m5_fe <- gamlss(fev ~ pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             sigma.fo = ~pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             nu.for = ~1, tau.fo = ~1, family = BCPEo, data = dfe, n.cyc = 100)


GAIC(m3_fe, m4_fe, m5_fe, k = log(nrow(dfe)))



# -->
# m3_fe has the lowest SBC



# ------------------------------------------------------------------------------
# Check the adequacy of model m3_fe using residual diagnostics
# ------------------------------------------------------------------------------

plot(m3_fe)



# ----------
wp(m3_fe, ylim.all = 0.6)

wp(m3_fe, xvar = ~age, n.inter = 9, ylim.worm = 0.8)

wp(m3_fe, xvar = ~height, n.inter = 9, ylim.worm = 0.8)

wp(m3_fe, xvar = ~age + height, n.inter = 4, ylim.worm = 1)



# ----------
Q.stats(m3_fe, xvar = dm$height, n.inter = 25)



# ------------------------------------------------------------------------------
# Effective degrees of freedom (including 2 for the constant and linear terms)
# ------------------------------------------------------------------------------

edfAll(m3_fe)



# ------------------------------------------------------------------------------
# Fitted smooth functions
# ------------------------------------------------------------------------------

term.plot(m3_fe, what = "mu", pages = 1, ask = FALSE)

term.plot(m3_fe, what = "sigma", pages = 1, ask = FALSE)



# ------------------------------------------------------------------------------
# Search for best effective degrees of freedom by minimizing a global SBC by find.hyper()
# ------------------------------------------------------------------------------

# This should use cubic splines instead of penalized splines.

### THIS TAKES TIME !!!: 16 minutes ###

k1 <- log(nrow(dfe))


mod_fe <- quote(gamlss(fev ~ cs((log(height)), df = p[1]) + cs((log(age)), df = p[2]),
                    sigma.fo = ~cs((log(height)), df = p[3]) + cs((log(age)), df = p[4]),
                    nu.fo = ~1, tau.fo = ~1,
                    family = BCTo, data = dfe, control = gamlss.control(trace = FALSE, n.cyc = 100)))


best_fe <- find.hyper(model = mod_fe, par = c(6, 4, 3, 3), lower = c(0.01, 0.01, 0.01, 0.01), steps = c(0.1, 0.1, 0.1, 0.1), k = k1)


best_fe
edfAll(m3_fe)


# -->
# The resulting effective degrees of freedom are very similar to model m3_fe ... ?



# ------------------------------------------------------------------------------
# Fit a model for height against age to find lower and upper centile limits of height for each age
# ------------------------------------------------------------------------------

k1 <- log(nrow(dfe))


mh_fe <- gamlss(height ~ pb(log(age), method = "GAIC", k = k1),
             sigma.fo = ~pb(log(age), method = "GAIC", k = k1),
             nu.fo = ~pb(log(age), method = "GAIC", k = k1),
             tau.fo = ~pb(log(age), method = "GAIC", k = k1),
             family = BCTo, data = dfe)


mh_fe

term.plot(mh_fe)

term.plot(mh_fe, what = "sigma")



# ------------------------------------------------------------------------------
# Plot the centiles for height against age
# ------------------------------------------------------------------------------

centiles(mh_fe, xvar = dfe$age, cent = c(0.1, 0.4, 2, 10, 25, 50, 75, 90, 98, 99.6, 99.9), ylab = "height", xlab = "age", legend = FALSE)



# ------------------------------------------------------------------------------
# Find lower (0.1%) and upper (99.9%) limits for height given age
# ------------------------------------------------------------------------------

newage <- seq(5, 90, 0.1)

newcent <- c(0.1, 50, 99.9)

maty_fe <- centiles.pred(mh_fe, xname = "age", xvalues = newage, cent = newcent, plot = TRUE)

maty_fe[1:10,]



# ------------------------------------------------------------------------------
# Construct a contour plot of the 5th centile of fev against height and age
# ------------------------------------------------------------------------------

# Expand a grid of values of age from 5 to 90 years and height from 100 to 210 cm to cover the limits of height
newdata <- expand.grid(age = seq(5, 90, 0.1), height = seq(100, 210, 1))


# Use the chosen model m3_fe for fev to predict all the parameters mu, sigma, nu and tau of the distribution BCTo for the values of age and height in newdata.
m3p_fe <- predictAll(m3_fe, newdata = newdata, type = "response")


m3p_fe


# Calculate the 5th centile of fev for all cases in newdata
fev5_fe <- qBCPE(0.05, m3p_fe$mu, m3p_fe$sigma, m3p_fe$nu, m3p_fe$tau)



# ----------
# For all cases of newdata with values of height outside the lower (0.1%) and upper (99.9%) bounds for height,
# replace the value of fev5 with a missing value (NaN)

lower <- rep(maty[,2], 111)

upper <- rep(maty[,4], 111)

fev5a_fe <- ifelse(((newdata$height < lower) | (newdata$height > upper)), NaN, fev5_fe)



# ----------
# Obtain a contour plot of the 5th centile of fev against height and age.
newheight <- seq(100, 210, 1)

newage <- seq(5, 90, 0.1)

mfev5_fe <- matrix(data = fev5a_fe, nrow = 851, ncol = 111)


# male and female
par(mfrow=c(1,2))
contour(newage, newheight, mfev5, nlevels = 40, xlab = "age(years)", ylab = "height(cm)", main = "male")
contour(newage, newheight, mfev5_fe, nlevels = 40, xlab = "age(years)", ylab = "height(cm)", main = "female")





