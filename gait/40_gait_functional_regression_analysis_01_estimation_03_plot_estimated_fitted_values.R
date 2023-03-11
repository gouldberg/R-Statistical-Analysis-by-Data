setwd("//media//kswada//MyFiles//R//gait")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gait
# ------------------------------------------------------------------------------

data("gait", package = "fda")

str(gait)

dim(gait)




# ------------------------------------------------------------------------------
# Fitted mean Knee angle
# ------------------------------------------------------------------------------

kneefdMean = mean(kneefd)

( kneeMean = predict(kneefdMean, gaitfine) )



# ----------
par(mfrow = c(1,1))

plot(kneeMean, type = "o")



# ------------------------------------------------------------------------------
# Case by Case:  fitted knee and mean
# ------------------------------------------------------------------------------

kneemat     <- eval.fd(gaitfine, kneefd)

kneehatmat  <- predict(gaitRegress$yhatfd$fd, gaitfine)


graphics.off()

op <- par(mfrow=c(3,3), ask=FALSE)

for (i in 1:ncurve) {

  # smoothed knee angle
  plot(gaitfine, kneemat[,i], type="l", lty=2, col="black", ylim=c(0,80))
  # mean function
  lines(gaitfine, kneeMean,           lty=2, col="black", lwd = 2)
  # fit function
  lines(gaitfine, kneehatmat[,i],        lty=1, col="blue", lwd = 2)
  title(paste("Case",i))
}

par(op)

