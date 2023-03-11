# setwd("//media//kswada//MyFiles//R//divorce//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//divorce//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  divorce
# ------------------------------------------------------------------------------

source("divorce_data.R")

length(dat)

data.ls <- list(J = length(dat), tt = dat)

table(dat)



# ------------------------------------------------------------------------------
# Density of weibull distribution
#   - x:  survival time,  the period until the some events occur
#   - shape = 1:  exponential distribution
#   - shape = 2:  Rayleigh distribution
#   - shape 3 to 4:  close to normal distribution
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))


# ----------
# shape paramter
m <- 3.5

# location parameter
eta <- c(2, 3, 4, 5, 6)

plot(0, 1, xlim=c(0, 10), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", xlab="x", cex.axis=1.6, cex.lab=1.6, type = "n", 
     main = paste0("shape:", m, " + locations changes"))

for(i in 1:length(eta)){
  par(new = T)
  curve(dweibull(x, shape=round(mean(m),3), scale=round(mean(eta[i]),3)),
        xlim=c(0, 10), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", xlab="x", col = i, cex.axis=1.6, cex.lab=1.6)
}



# ----------
# shape paramter
m <- c(0.5, 1, 1.5, 3.5, 4, 5)

# location parameter
eta <- 2

plot(0, 1, xlim=c(0, 10), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", xlab="x", cex.axis=1.6, cex.lab=1.6, type = "n", 
     main = paste0("location:", eta, " + shape changes"))

for(i in 1:length(m)){
  par(new = T)
  curve(dweibull(x, shape=round(mean(m[i]),3), scale=round(mean(eta),3)),
        xlim=c(0, 10), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", xlab="x", col = i, cex.axis=1.6, cex.lab=1.6)
}



# ------------------------------------------------------------------------------
# Survival function,  reliablity function
#   - the probability that the subject is survived up to the period
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,2))

x <- seq(0, 10, by = 0.01)


# ----------
# shape paramter
m <- 3.5

# location parameter
eta <- c(2, 3, 4, 5, 6)

plot(0, 1, xlim=c(0, 10), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", xlab="x", cex.axis=1.6, cex.lab=1.6, type = "n", 
     main = paste0("Survival Func   shape:", m, " + locations changes"))

for(i in 1:length(eta)){
  par(new = T)
  curve(exp(-(x / eta[i]) ^ m),
        xlim=c(0, 10), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", xlab="x", col = i, cex.axis=1.6, cex.lab=1.6)
}



# ----------
# shape paramter
m <- c(0.5, 1, 1.5, 3.5, 4, 5)

# location parameter
eta <- 2

plot(0, 1, xlim=c(0, 10), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", xlab="x", cex.axis=1.6, cex.lab=1.6, type = "n", 
     main = paste0("Survival Func   location:", eta, " + shape changes"))

for(i in 1:length(m)){
  par(new = T)
  curve(exp(-(x / eta) ^ m[i]),
        xlim=c(0, 10), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", xlab="x", col = i, cex.axis=1.6, cex.lab=1.6)
}



# ------------------------------------------------------------------------------
# Harazard function, hazard rate at the time
#   - scale = 1:  hazard risk is constant:  guuhatu - koshou type
#   - 0 < scale < 1:  shoki - koshou  type
#   - scale > 1:  mamou - koshou type
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,2))


# ----------
# shape paramter
m <- 3.5

# location parameter
eta <- c(2, 3, 4, 5, 6)

plot(0, 1, xlim=c(0, 3), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", xlab="x", cex.axis=1.6, cex.lab=1.6, type = "n", 
     main = paste0("Hazard Func   shape:", m, " + locations changes"))

for(i in 1:length(eta)){
  par(new = T)
  curve(dweibull(x,shape=round(mean(m),3), scale=round(mean(eta[i]),3)) / (1-pweibull(x,shape=round(mean(m),3), scale=round(mean(eta[i]),3))),
        xlim=c(0, 3), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", col = i, cex.axis = 1.6, cex.lab = 1.6)
}



# ----------
# shape paramter
m <- c(0.5, 1, 1.5, 3.5, 4, 5)

# location parameter
eta <- 2

plot(0, 1, xlim=c(0, 3), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", xlab="x", cex.axis=1.6, cex.lab=1.6, type = "n", 
     main = paste0("Hazard Func   location:", eta, " + shape changes"))

for(i in 1:length(m)){
  par(new = T)
  curve(dweibull(x,shape=round(mean(m[i]),3), scale=round(mean(eta),3)) / (1-pweibull(x,shape=round(mean(m[i]),3), scale=round(mean(eta),3))),
        xlim=c(0, 3), ylim=c(0, 1.0), lty = i, lwd = 2, ylab="", col = i, cex.axis = 1.6, cex.lab = 1.6)
}

