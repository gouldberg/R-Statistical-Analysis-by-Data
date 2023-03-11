setwd("//media//kswada//MyFiles//R//cairo")

packages <- c("dplyr", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cairo
# ------------------------------------------------------------------------------

data("cairo", package = "gamair")

str(cairo)

head(cairo)



# ----------
car::some(cairo)



# ------------------------------------------------------------------------------
# model with corAR1 and cyclic smoothers:  bam()
#
#   - The bam function also allows a simple AR1 model on the residuals (in data frame order).
#     It has the small advantage that we do not need to nest the AR1 correlation structure within year,
#     but the disadvantage that estimation of the correlation parameter rho is not automatic.
# ------------------------------------------------------------------------------


( REML <- rho <- 0.6 + 0:20 / 100 )



for(i in 1:length(rho)){
  ctbam <- bam(temp ~ s(day.of.year, bs = "cc", k = 20) + s(time, bs = "cr"), data = cairo, rho = rho[i])

  REML[i] <- ctbam$gcv.ubre
}



# ----------
plot(REML ~ rho, type = "b")

rho[which.min(REML)]


# -->
# The minimim REML is at rho = 0.69



# ----------
# refit by best rho
ctbam <- bam(temp ~ s(day.of.year, bs = "cc", k = 20) + s(time, bs = "cr"), data = cairo, rho = rho[which.min(REML)])



# ----------
plot(ctbam, scale = 0)



# -->
# It seems that there is evidence for a long term trend in temperature, and that the model fits fairly closely.



