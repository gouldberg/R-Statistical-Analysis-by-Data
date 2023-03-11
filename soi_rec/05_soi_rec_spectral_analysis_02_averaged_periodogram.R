setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data:  rec
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")





# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------


# L <- floor(2 * sqrt(nextn(length(soi))))

L <- 9

m <- (L - 1) / 2


kernel("daniell", m)



# ----------
par(mfrow=c(2,1))


soi.ave <- astsa::mvspec(soi, kernel("daniell", m), log = "no")
abline(v = c(0.25, 1, 2, 3), lty = 2)


rec.ave <- astsa::mvspec(rec, kernel("daniell", m), log = "no")
abline(v = c(0.25, 1, 2, 3), lty = 2)





# ------------------------------------------------------------------------------
# Confidence Intervals of the averaged periodogram
# ------------------------------------------------------------------------------

# df:  degrees of freedom adjusted by L
# = 2 * L * 453 / 480 ~ 17


( df <- soi.ave$df )


# ----------
# Confidence Intervals at the yearly cycle omega = 1/12 and 1/48
# now the df is increased from 2 to 17,
# the confidence interval is narrowed


( U <- qchisq(0.025, df) )

( L <- qchisq(0.975, df) )


df * soi.ave$spec[40] / L

df * soi.ave$spec[40] / U

df * rec.ave$spec[40] / L

df * rec.ave$spec[40] / U



# ----------
df * soi.ave$spec[10] / L

df * soi.ave$spec[10] / U

df * rec.ave$spec[10] / L

df * rec.ave$spec[10] / U




# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram on log scale
# ------------------------------------------------------------------------------

# log = "yes"
par(mfrow=c(2,1))

soi.ave <- astsa::mvspec(soi, kernel("daniell", m), log = "yes")
abline(v = c(0.25, 1, 2, 3), lty = 2)

rec.ave <- astsa::mvspec(rec, kernel("daniell", m), log = "yes")
abline(v = c(0.25, 1, 2, 3), lty = 2)



# -->
# when log = "yes"
# cross mark is shown upper right corner.
# the width = bandwidth (here 0.225)
# the vertical bar = apprimate 95% confidence interval


