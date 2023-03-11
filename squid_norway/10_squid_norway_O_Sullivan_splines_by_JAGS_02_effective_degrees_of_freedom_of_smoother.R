setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ----------
# for comparison with MCMC approach later, we standardize the covariate
Mystd <- function(x) {(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}

Squid$Lat.std <- Mystd(Squid$Lat)
Squid$ML.std  <- Mystd(Squid$ML)



# ------------------------------------------------------------------------------
# Effective degrees of freedom of a smoother of O'sullivan spline in JAGS
# ------------------------------------------------------------------------------

XFixed <- model.matrix(~ 1 + Lat.std, data = Squid)    

numIntKnots <- 11

probs <- seq(0, 1, length = (numIntKnots + 2))[-c(1, (numIntKnots + 2))]

intKnotsTime <- quantile(unique(Squid$ML.std), probs)

XZML <- OSullivan(Squid$ML.std, 
                  numIntKnots = numIntKnots, 
                  AddIntercept = FALSE,
                  intKnots = intKnotsTime)


# ----------
# C:
C <- cbind(XFixed, win.data3$Xspl, win.data3$Z)


# D:
D <- diag(ncol(win.data3$Z)+3)
D[1,1] <- 0
D[2,2] <- 0
D[3,3] <- 0


# lambda:
lambda <- out$mean$sigma^2 / out$mean$sigma.u^2
lambda <- as.numeric(lambda) 



# ----------
# estimated smoother f = Something * y
# Effective degrees of freedom is the rank of the "Something"
Something <- C %*% solve(t(C) %*% C + lambda * D) %*% t(C)
sum(diag(Something))


# -->
# The effective of all the terms in the model is 6.079
# Given that the model also contains an intercept and a lattitude effect means that the smoother consumes 4.079 edf.



# ----------
# We can obtain the 4.079 directly
Someth <- solve(t(C) %*% C + lambda * D) %*% t(C)
xx <- C[,3:16] %*% Someth[3:16,]
sum(diag(xx))



# ------------------------------------------------------------------------------
# Plot O'Sullivan splines smoother
# ------------------------------------------------------------------------------
par(mar = c(5,5,2,2), cex.lab = 1.5)  

# O'Sullivan MCMC smoother
plot(x = ML.100, 
     y = f1.info[,4], 
     type = "l", 
     xlab = "Standardized ML",
     ylab = "Smoother",
     ylim = c(-2,2), cex.lab = 1.5)
lines(ML.100, f1.info[,1], lty=2)
lines(ML.100, f1.info[,3], lty=2)


# O'Sullivan smoother
# bu:  estimated parameters by O'Sullivan splines = psterior means obtained by MCMC
bu     <- solve(t(C) %*% C + lambda * D) %*% t(C) %*% win.data3$Y
Fit    <- C %*% bu
f_lambda <- cbind(XZML100$X, XZML100$Z) %*% bu[3:16]
lines(ML.100, f_lambda, lty=2, col = 1, lwd = 2)

legend("topleft",
       legend = c("Smoother based on MCMC",
                  "Smoother based on Equation (1.20)"),
       lwd = c(1,2),
       lty = c(1,2),
       cex = 1.2)
