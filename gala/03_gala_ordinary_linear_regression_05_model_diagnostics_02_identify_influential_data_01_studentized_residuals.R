]setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)




# ----------
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)


# Baltra deleted
lmod_del <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala[-1,])




# ------------------------------------------------------------------------------
# Identify influential observations:  studentized residuals
# ------------------------------------------------------------------------------


rstudent(lmod)




# ----------
# estimate of sigma^2

( sigma2 <- sum(lmod$residuals^2) * (1 / (30 - 6)) )


( sigma2_del <- sum(lmod_del$residuals^2) * (1 / (30 - 1 - 6)) )




# ----------
# studentized residuals is some standardized residuals using "hat"


lmod$residuals[1] / sqrt( sigma2_del * (1 - hat[1]) )


lmod$residuals[1] / sqrt( sum(lmod$residuals^2) * (1 - hat[1]) - lmod$residuals[1]^2 ) * sqrt((30 - 1 - 6))


rstudent(lmod)[1]



# for reference:  difference of sigma^2
sigma2_del - sigma2




# ---------
# Calculation of sigma2_del


# standardized residuals for Beltra
( rst <- lmod$residual[1] / sqrt(sigma2 * (1 - hat[1]) ) )

( rst <- rstandard(lmod)[1] )


sigma2 * ( 30 - 6 - rst^2 )  / ( 30 - 1 - 6 )

sigma2_del




# ------------------------------------------------------------------------------
# Plot studentized residuals  (compared to standardized residuals)
# ------------------------------------------------------------------------------


par(mfrow = c(2,1))

plot(rstudent(lmod), type = "h")

abline(h = c(-2, 2), col = "gray", lty = 2)

plot(rstandard(lmod), type = "h")

abline(h = c(-2, 2), col = "gray", lty = 2)



# ----------
crit <- 2


rstudent(lmod)[abs(rstudent(lmod)) >= crit]




