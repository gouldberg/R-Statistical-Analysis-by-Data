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


age <- growth$age


# ----------
hgtmfit <- eval.fd(age,     hgtmfd)

hgtmhat <- eval.fd(agefine, hgtmfd)

velmhat <- eval.fd(agefine, hgtmfd, 1)

accmhat <- eval.fd(agefine, hgtmfd, 2)



# ----------
children <- c(1, 2, 4, 6, 9, 10, 12)

par(mfrow=c(2,2), pty="s", ask=TRUE)

for (i in children) {
  plot(age, hgtm[,i], ylim=c(60,200), xlab="Years", ylab="", main=paste("Height for male",i))
  lines(agefine, hgtmhat[,i], col=2)
  
  resi <- hgtm[,i] - hgtmfit[,i]
  ind  <- resi >= -.7 & resi <= .7
  plot(age[ind], resi[ind], type="b", ylim=c(-.7,.7), xlab="Years", ylab="", main="Residuals")
  abline(h=0, lty=2)
  
  ind <- velmhat[,i] >= 0 & velmhat[,i] <= 20
  plot(agefine[ind], velmhat[ind,i], type="l", ylim=c(0,20), xlab="Years", ylab="", main="Velocity")
  abline(h=0, lty=2)
  
  ind <- accmhat[,i] >= -6 & accmhat[,i] <= 6
  plot(agefine[ind], accmhat[ind,i], type="l", ylim=c(-6,6), xlab="Years", ylab="", main="Acceleration")
  abline(h=0, lty=2)
}    



# ------------------------------------------------------------------------------
# Female:  Plot smoothed, residuals, velocity and acceleration curves by each individuals
# ------------------------------------------------------------------------------

hgtffit <- eval.fd(age,     hgtffd)

hgtfhat <- eval.fd(agefine, hgtffd)

velfhat <- eval.fd(agefine, hgtffd, 1)

accfhat <- eval.fd(agefine, hgtffd, 2)



# ----------
children <- c(1, 2, 4, 6, 9, 10, 12)

par(mfrow=c(2,2), pty="s", ask=TRUE)

for (i in children) {
  plot(age, hgtf[,i], ylim=c(60,200), xlab="Years", ylab="", main=paste("Height for female",i))
  lines(agefine, hgtfhat[,i], col=2)

  resi <- hgtf[,i] - hgtffit[,i]
  ind  <- resi >= -.7 & resi <= .7
  plot(age[ind], resi[ind], type="b", ylim=c(-.7,.7), xlab="Years", ylab="", main="Residuals")
  abline(h=0, lty=2)
  
  ind <- velfhat[,i] >= 0 & velfhat[,i] <= 20
  plot(agefine[ind], velfhat[ind,i], type="l", ylim=c(0,20), xlab="Years", ylab="", main="Velocity")
  abline(h=0, lty=2)
  
  ind <- accfhat[,i] >= -6 & accfhat[,i] <= 6
  plot(agefine[ind], accfhat[ind,i], type="l", ylim=c(-6,6), xlab="Years", ylab="", main="Acceleration")
  abline(h=0, lty=2)
}
