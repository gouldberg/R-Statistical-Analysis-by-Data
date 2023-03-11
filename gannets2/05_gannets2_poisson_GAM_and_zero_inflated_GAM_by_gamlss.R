setwd("//media//kswada//MyFiles//R//gannets2")

packages <- c("dplyr", "lattice", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gannets2
# ------------------------------------------------------------------------------

Gannets <- read.table(file = "Gannets2.txt", header = TRUE)


str(Gannets)
names(Gannets)


Gannets$Xkm <- Gannets$X / 1000
Gannets$Ykm <- Gannets$Y / 1000
Gannets2 <- Gannets[Gannets$Area_surveyedkm2 > 0,]
Gannets2$LArea <- log(Gannets2$Area_surveyedkm2)
Gannets2$TimeH <- Gannets2$Hours + Gannets2$Minutes/60
Gannets2$Date <- paste(Gannets2$Day,Gannets2$Month,Gannets2$Year, sep = "/")
Gannets2$DayInYear <- strptime(Gannets2$Date, "%d/%m/%Y")$yday + 1
Gannets2$DaySince0 <- ceiling(julian(strptime(Gannets2$Date, format = "%d/%m/%Y"), origin = as.Date("1991-01-01")))
Gannets2$G     <- Gannets2$Gannets_in_transect
Gannets2$GProp <- Gannets2$G / Gannets2$Area



# ------------------------------------------------------------------------------
# Fit Zero-inflated GAM
#   - Spatial correlation may be confounded with zero-inflation; neighbouring transects may have zero gannet abundance.
#     To take this into account we can eigher apply a GAM model with a spatial correlation structure (e.g. a so-called CAR correlation) or apply
#     a zero-inflated GAM. A model that contains both components is likely to cause numerical problems, for instance non-mixing chains in
#     Bayesian estimation techniques.
#   - This means that we have to choose between the two approaches, and this choice should be based on biological arguments.
#
#   - Ordinary ZIP GLM or zero-inflated negative binomial (ZINB) GLM can be executed in R with the function zeroinfl available in the package pscl,
#     but it cannot cope directly with smoothers.
#     Packages that can execute zero-inflated GAMs are COZIGAM, VGAM and gamlss.
#     In our experience VGAM is a useful package, but at times is confusing to work with.
#     We tried COZIGAM for the gannet data but encountered numerical estimation problems.
#     We will use the package gamlss, yet a different approach is to program our own splines and use these in the function zeroinfl from the pscl package.
#   - One of the major advantages of gamlss is that it can deal with a much wider range of distributions than the glm function in R or the gam function
#     in mgcv. Also, gamlss allows modelling the variance as a function of covariates, but we will not make use of this for the ganne example.
# ------------------------------------------------------------------------------
library(gamlss)
library(gamlss.add)


# Fit the equivalent of M8: Poisson GAM
# The package gamlss does not do cross-validation as in mgcv, but it does allow the user to specify multiple models and compare them via the AIC.
# The function gamlss cannot do 2-dimensional smoothers, but an advantage is the function ga() that allows for a interface to the function gam from mgcv.
# We also tried the te function, but this gave a list of warning and error messages.
# The problem is that s(Xkm, YKm) assumes that both variables have the same scale and variation.
# Rescaling them to between 0 and 1 is not an option, as it will distrot the spatial position of the sites, so we are faced with a challenge.
M9 <- gamlss(G ~ cs(Year, df = 8) + cs(TimeH, df = 8) + cs(DayInYear, df = 8) + ga(~s(Xkm, Ykm, fx = TRUE, k = 28)) + factor(Seastate) + offset(LArea),
      family = PO(), data = Gannets2)

summary(M9)



# ----------
# In gamlss, the functions to obtain the estimated smoothers are different from those of mgcv; 
# gamlss does not use deviance or Pearson residuals but randomized quantile residuals.

plot(M9)

termplot(M9)



# ----------
# ZIP GAM
# The model converges after about 50 iterations. The default setting stops at 20 iterations; hence the gamlss.control code.
con <- gamlss.control(n.cyc = 200)


# ga() allows for an interface to the funcction gam from mgcv
# ga(~s()) applies a 2-dimensional smoother using the s()  (we can try te())
M10 <- gamlss(G ~  cs(Year, df = 8) + cs(TimeH, df = 8) + cs(DayInYear, df = 8) + ga(~s(Xkm, Ykm, fx = TRUE, k = 28)) + factor(Seastate) + offset(LArea), family = ZIP(),
              data = Gannets2, control = con)


# ----------
# Extract the estimated Pi
Pi <- fitted (M10, "sigma")[1]
Pi


# -->
# The probability that a zero is a false zero is 0.75, a high values.



# ----------
# Calculate overdispersion 
pi   <- M10$sigma.fv[1]
mu   <- M10$mu.fv
ExpY <- (1 - pi) * mu
varY <-  mu * (1 - pi) * (1 + mu * pi)
PearsonRes <- (Gannets2$G - ExpY) / sqrt(varY)
N    <- nrow(Gannets2)
p    <- M10$df.fit

Overdispersion <- sum(PearsonRes^2) / (N - p)

Overdispersion



# -->
# ZIP GAM is still overdispersed, which is surprising, as we expect that we have high bird abundances (which require a negative binomial distribution)
# and zero-inflation.


