# setwd("//media//kswada//MyFiles//R//hawaii")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//hawaii")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hawaii
# ------------------------------------------------------------------------------

Hawaii <- read.table(file = "Hawaii.txt", header = TRUE)


str(Hawaii)


dim(Hawaii)


car::some(Hawaii)


# ----------
Hawaii$Birds <- sqrt(Hawaii$Moorhen.Kauai)



# ------------------------------------------------------------------------------
# Impose correlation structure:  corARMA
#   - Check the correlation matrix
# ------------------------------------------------------------------------------


# corARMA

cs <- corARMA(value = c(0.3, -0.3), p = 2, q = 0, form = ~Year)


# cs <- Initialize(cs, Hawaii)



# ----------
coef(cs, unconstrained = FALSE)


coef(cs)


corMatrix(cs, covariate = 1:4)




# ------------------------------------------------------------------------------
# Impose correlation structure:  corARMA
#   - But remember the citation from Schabenberger and Pierce (2002): there is NOT much to be gained
#     from finding the perfect correlation structure compared to finding one that is adequate
# ------------------------------------------------------------------------------


library(nlme)


# same as corAR1
( cs1 <- corARMA(c(0.2), p = 1, q = 0, form = ~ Year) )



# ( cs2 <- corARMA(c(0.9, -0.3), p = 2, q = 0, form = ~ Year) )

( cs2 <- corARMA(p = 2, q = 0, form = ~ Year) )



M3arma1 <- gls(Birds ~ Rainfall + Year, na.action = na.omit, data = Hawaii, 
          correlation = cs1)



M3arma2 <- gls(Birds ~ Rainfall + Year, na.action = na.omit, data = Hawaii, 
               correlation = cs2)



# ----------
summary(M3arma1)


summary(M3arma2)



# -->
# The estimated auto-regressive parameters of the ARMA(2,0) model where phi1 = 0.99 and phi2 = -0.35.
# The value for phi1 close to 1 may indicate a more serious problem of the residuals being non-stationary
# (non-constant mean or variance)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------

compareCoefs(M0, M1, M2, M3arma1, M3arma2)




# ----------
AIC(M0, M1, M2, M3arma1, M3arma2)



# -->
# Note that NO much is gained from complex corARMA model



# ------------------------------------------------------------------------------
# model comparison:  residuals
# ------------------------------------------------------------------------------

# but almost same residuals

plot(M2)

plot(M3arma1)

plot(M3arma2)




# ------------------------------------------------------------------------------
# Assess residual pattern
# ------------------------------------------------------------------------------

E <- residuals(M3arma2, type = "normalized")


par(mfrow = c(1,1))
plot(E, type = "o")


# plot(M3arma2)


# -->
# The error structure that allows for a sinusoidal pattern may be more appropriate



# ------------------------------------------------------------------------------
# Auto-correlation plot for residuals
# ------------------------------------------------------------------------------

I1 <- !is.na(Hawaii$Birds)

Efull <- vector(length = length(Hawaii$Birds))

Efull <- NA

Efull[I1] <- E


acf(Efull, na.action = na.pass, main = "Auto-correlation plot for residuals")



