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
# Linear spline regression in JAGS:  assess model, parameter distribution
# ------------------------------------------------------------------------------
MyBUGSOutput <- function(Output  = Output, SelectedVar = SelectedVar, VarNames = NULL){
  xx   <- Output
  vars <- SelectedVar
  
  if (is.null(VarNames)) { VarNames <- SelectedVar }
  if (length(SelectedVar) != length(VarNames)) {stop("Wrong number of variable names")}
  
  x <- xx$sims.matrix
  OUT <- matrix(nrow = length(vars), ncol=4) 
  j<-1
  for(i in vars){
    xi <- x[,i]	
    OUT[j,3:4] <- quantile(xi, probs = c(0.025, 0.975))
    OUT[j,1]   <- mean(xi)
    OUT[j,2]   <- sd(xi)
    j          <- j + 1
  }
  colnames(OUT) <- c("mean", "se", "2.5%", "97.5%")
  rownames(OUT) <- VarNames
  OUT
}

OUT1 <- MyBUGSOutput(out, c(uNames("beta", ncol(X)), "sigma"))

print(OUT1, digits =3)



# ------------------------------------------------------------------------------
# Linear spline regression in JAGS:  assess model, posterior means and standard errors
# ------------------------------------------------------------------------------
library(coefplot2)  # is NOT available for R 3.6.0
beta5 <- coef(M5)[2:6]
se5   <- sqrt(diag(vcov(M5)[2:6, 2:6]))

beta1 <- OUT1[2:6,1]
se1   <- OUT1[2:6,2]

par(mar = c(5,5,2,2))
coefplot2(beta5, se5, offset = 0, col =1, varnames = names(beta5), xlim = c(-2,3), cex.lab = 1.5, cex.var = 1)
coefplot2(beta1, se1, offset = 0.15, col = 1, varnames = names(beta5), add = TRUE)




# Access posterior mean residuals
E <- out$mean$E



# ------------------------------------------------------------------------------
# Linear spline regression in JAGS:  assess model, posterior mean smoother and 95% credible intervals.
# ------------------------------------------------------------------------------
# Extract coefficients
beta <- out$sims.list$beta  #ML smoother
dim(beta)

range(Squid$ML.std)
NewData <- data.frame(Lat.std = 0, ML.std =  seq( -1.4,  3, length = 100))

Xnew <- model.matrix(~ Lat.std + ML.std  + rhs(ML.std, -0.7121410) + rhs(ML.std, -0.1667513) + rhs(ML.std, 0.6419299),  data = NewData)


# Calculate the smoothers
f <-  Xnew[,3:6] %*% t(beta[,3:6])


MySmoother <- function(f) {
  # Get the posterior mean, and 95% credible interval
  Sm1 <- matrix(nrow = 100, ncol = 5)
  for (i in 1:100) {
    Sm1[i,1:3] <- quantile(f[i,], probs = c(0.025, 0.5, 0.975))
    Sm1[i,4]  <- mean(f[i,])
    Sm1[i,5]  <- sd(f[i,])
  } 	
  colnames(Sm1) <- c("Lower", "Median", "Upper", "Mean", "SE")
  Sm1
}


f.info <- MySmoother(f)


par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
plot(x = NewData$ML.std, y = f.info[,4], type = "l",  xlab = "Standardized ML", ylab = "Smoother", ylim = c(-3, 2), cex.lab= 1.5,  lwd = 2)
lines(NewData$ML.std, f.info[,1], lty=2, lwd = 2)
lines(NewData$ML.std, f.info[,3], lty=2, lwd = 2)

