setwd("//media//kswada//MyFiles//R//eqexp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqexp
#   - Various bivariate earthquakes (EQ) and explosions (EX) recorded at 40 pts/s including an even NZ (Novaya Zemlya)
# ------------------------------------------------------------------------------

data(eqexp, package = "astsa")


head(eqexp)

str(eqexp)


attach(eqexp)



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

P <- 1:1024

S <- P + 1024


x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S], EX6[S], NZ[S])

x.name <- c("EQ5-P", "EQ6-P", "EX5-P", "EX6-P", "NZ-P", "EQ5-S", "EQ6-S", "EX5-S", "EX6-S", "NZ-S")

colnames(x) <- x.name



# ----------
graphics.off()
par(mfrow = c(1,1))
plot.ts(x, main = "")

mtext("P waves", side = 3, line = 1.2, adj = 0.05, cex = 1.2)
mtext("S waves", side = 3, line = 1.2, adj = 0.85, cex = 1.2)



# -->
# Various bivariate earthquakes (EQ) and explosions (EX) recorded at 40 pts/s
# compared with an even NZ (Novaya Zemlya) of unknown origin.

# Compressional waves, also known as primary or P waves, travel fastest in the Earth's crust and are first to arrive.
# Shear waves propagate more slowly through the Earth and arrive second, hence they are called secondary or S waves.


# By seeing time series plot,
# Wave of NZ-P and NZ-S are resembled to explosion



# ----------
apply(x, MARGIN = 2, FUN = forecast::ndiffs)





# ------------------------------------------------------------------------------
# data exploration:  Compare time series plot   EX1, EX3, EX8  vs. other earth quakes
# ------------------------------------------------------------------------------


x <- cbind(EQ4[P], EQ6[P], EX1[P], EX3[P], EX8[P], EQ4[S], EQ6[S], EX1[S], EX3[S], EX8[S])

x.name <- c("EQ4-P", "EQ6-P", "EX1-P", "EX3-P", "EX8-P", "EQ4-S", "EQ6-S", "EX1-S", "EX3-S", "EX8-S")

colnames(x) <- x.name



# ----------
graphics.off()
par(mfrow = c(1,1))
plot.ts(x, main = "")

mtext("P waves", side = 3, line = 1.2, adj = 0.05, cex = 1.2)
mtext("S waves", side = 3, line = 1.2, adj = 0.85, cex = 1.2)




# -->
# Note that time series of EX1, EX3, EX8 are quite resemble to time series of earth quakes (EQ4, EQ6)




# ------------------------------------------------------------------------------
# data exploration:  Compare time series plot   NZ  vs. other explosions (EX4, EX5, EX6)
# ------------------------------------------------------------------------------


x <- cbind(NZ[P], EX4[P], EX5[P], EX6[P], NZ[S], EX4[S], EX5[S], EX6[S])

x.name <- c("NZ-P", "EX4-P", "EX5-P", "EX6-P", "NZ-S", "EX4-S", "EX5-S", "EX6-S")

colnames(x) <- x.name



# ----------
graphics.off()
par(mfrow = c(1,1))
plot.ts(x, main = "")

mtext("P waves", side = 3, line = 1.2, adj = 0.05, cex = 1.2)
mtext("S waves", side = 3, line = 1.2, adj = 0.85, cex = 1.2)




# -->
# Note that time series of NZ is quite resembled to time series of EX4 and EX5
)

