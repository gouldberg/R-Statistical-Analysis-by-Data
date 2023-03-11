setwd("//media//kswada//MyFiles//R//beamd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  beamd
# ------------------------------------------------------------------------------

data(beamd, package = "astsa")

str(beamd)

head(beamd)


sns1 <- beamd$sensor1
sns2 <- beamd$sensor2
sns3 <- beamd$sensor3



# ------------------------------------------------------------------------------
# Estimate time delays and align time series based on max lag in cross-correlation analysis
# ------------------------------------------------------------------------------

tau <- rep(0, 3)


# ----------
u <- ccf(sns1, sns2, plot = FALSE)

tau[1] <- u$lag[which.max(u$acf)]



# ----------
u <- ccf(sns3, sns2, plot = FALSE)

tau[3] <- u$lag[which.max(u$acf)]



# ----------
Y <- ts.union(stats::lag(sns1, tau[1]), stats::lag(sns2, tau[2]), stats::lag(sns3, tau[3]))



# ------------------------------------------------------------------------------
# Remove noise in individual channels and obtain common signals
# ------------------------------------------------------------------------------

# Common signal by averaging the aligned time series
Y <- ts.union(Y, rowMeans(Y))

colnames(Y) <- c("sensor1", "sensor2", "sensor3", "beamd")


plot.ts(Y)

