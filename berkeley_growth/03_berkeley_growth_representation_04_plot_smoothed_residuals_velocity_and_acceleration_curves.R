setwd("//media//kswada//MyFiles//R//berkeley_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  berkeley growth
#   - The data from the Berkeley Growth Study (Tuddenham and Snyder, 1954).
# ------------------------------------------------------------------------------

data("growth", package = "fda")

str(growth)



# ----------
# 39 boys and 31 points
# a 31 by 39 numeric matrix giging the heights in centimeters of 39 boys at 31 ages
dim(growth$hgtm)


# 54 girls and 31 points
# a 31 by 54 numeric matrix giging the heights in centimeters of 54 girls at 31 ages
dim(growth$hgtf)



# ----------
head(growth$hgtm)


head(growth$hgtf)



# ----------
growth$age


# -->
# The ages are not equally spaced;
# There are four measurements while the child is one year old, annual measurements from two to eight years,
# followed by heights measured biannually



# ------------------------------------------------------------------------------
# Male:  Plot smoothed, residuals, velocity and acceleration curves by each individuals
# ------------------------------------------------------------------------------

agefine <- seq(rng[1], rng[2], length=101)


children <- 1:ncasem


graphics.off()
par(mfrow=c(2,2), pty="s", ask=TRUE)

for(i in children){
  Wfd  <- fd(cvecm[,i], wbasis)
  beta <- betam[,i]
  hgtmfit <- beta[1] + beta[2]*monfn(age, Wfd)
  hgtmhat <- beta[1] + beta[2]*monfn(agefine, Wfd)
  velmhat <- beta[2] * eval.monfd(agefine, Wfd, 1)
  accmhat <- beta[2] * eval.monfd(agefine, Wfd, 2)
  
  plot(age, hgtm[,i], ylim=c(60,200), xlab="Years", ylab="", main=paste("Height for male",i))
  lines(agefine, hgtmhat, col=2)
  
  resi <- hgtm[,i] - hgtmfit
  ind  <- resi >= -.7 & resi <= .7
  plot(age[ind], resi[ind], type="b", ylim=c(-.7,.7), xlab="Years", ylab="", main="Residuals")
  abline(h=0, lty=2)
  
  ind <- velmhat >= 0 & velmhat <= 20
  plot(agefine[ind], velmhat[ind], type="l", ylim=c(0,20), xlab="Years", ylab="", main="Velocity")
  
  ind <- accmhat >= -6 & accmhat <= 6
  plot(agefine[ind], accmhat[ind], type="l", ylim=c(-6,6), xlab="Years", ylab="", main="Acceleration")
  abline(h=0, lty=2)
}



# ------------------------------------------------------------------------------
# Female:  Plot smoothed, residuals, velocity and acceleration curves by each individuals
# ------------------------------------------------------------------------------

children <- 1:ncasef


graphics.off()
par(mfrow=c(2,2),pty="s",ask=TRUE)

for(i in children){
  Wfd  <- fd(cvecf[,i],wbasis)
  beta <- betaf[,i]
  hgtffit <- beta[1] + beta[2] * monfn(age, Wfd)
  hgtfhat <- beta[1] + beta[2] * monfn(agefine, Wfd)
  velfhat <- beta[2] * eval.monfd(agefine, Wfd, 1)
  accfhat <- beta[2] * eval.monfd(agefine, Wfd, 2)
  
  plot(age, hgtf[,i], ylim=c(60,200), xlab="Years", ylab="", main=paste("Height for female",i))
  lines(agefine, hgtfhat, col=2)
  
  resi <- hgtf[,i] - hgtffit
  ind  <- resi >= -.7 & resi <= .7
  plot(age[ind], resi[ind], type="b", ylim=c(-.7,.7), xlab="Years", ylab="", main="Residuals")
  abline(h=0, lty=2)
  
  ind <- velfhat >= 0 & velfhat <= 20
  plot(agefine[ind], velfhat[ind], type="l", ylim=c(0,20), xlab="Years", ylab="", main="Velocity")
  
  ind <- accfhat >= -6 & accfhat <= 6
  plot(agefine[ind], accfhat[ind], type="l", ylim=c(-6,6), xlab="Years", ylab="", main="Acceleration")
  abline(h=0, lty=2)
}

