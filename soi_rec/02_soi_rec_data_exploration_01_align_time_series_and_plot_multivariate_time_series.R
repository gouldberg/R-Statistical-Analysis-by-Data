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
# data exploration:  check the time series length, start and end
# ------------------------------------------------------------------------------

length(soi);  length(rec);


head(soi, 6);  head(rec, 6);


tail(soi, 6);  tail(rec, 6);




# ------------------------------------------------------------------------------
# data exploration:  align time series and multivarite time series plot
# ------------------------------------------------------------------------------


# select intersect of time series and multivarite time series plot
( tmp <- ts.intersect(soi, rec) )



# ----------
graphics.off()

par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(1,1))

MTSplot(tmp)




# ----------
par(mfrow=c(2,1), mar = c(2,2,2,2))

plot(soi, ylab = "", xlab = "", main = "Southern Oscillation Index")
abline(v = seq(1950, 1990, by = 5), lty = 2, col = gray(0.4))

plot(rec, ylab = "", xlab = "", main = "Recruitment")
abline(v = seq(1950, 1990, by = 5), lty = 2, col = gray(0.4))





# -->
# The series show two basic oscillations types, an obvious annual cycle (hot in the summer, cold in the winter),
# and a slower frequency taht semmes th repeat about every 4 years.

# The two series are also related; it is easy to imagine the fish population is dependent on the ocean temperature.
# This possibility suggests trying some version of regression analysis as a procedure for relating the two series.
# Transfer function modelling can also be applied in this case.



