# setwd("//media//kswada//MyFiles//R//access_time//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//access_time//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  access time
# ------------------------------------------------------------------------------

data <- read.csv("access_time.csv", header = T)


str(data)


library(circular)


checkin_data <- as.circular(data, units = "hours", control.circular = list(modulo="2pi"))

for (i in 1:length(checkin_data)){
  x[i] <- as.vector((checkin_data[i]/24) * (2*pi))
}

head(x)


data <- list(N = nrow(x), x = x[,1])



# ------------------------------------------------------------------------------
# Assess the distribution of mean of 4 mixed models
# ------------------------------------------------------------------------------

mu1 <- extract(fit_4)$mu1

mu2 <- extract(fit_4)$mu2

mu3 <- extract(fit_4)$mu3

mu4 <- extract(fit_4)$mu4



# ----------
# EAP access time:  access time is concentrated on those EAP times

round(mean.circular(mu1, control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24

round(mean.circular(mu2, control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24

round(mean.circular(mu3, control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24

round(mean.circular(mu4, control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24



# ----------
pro <- c(0.025, 0.5, 0.975)   

round(quantile(mu1, probs = pro,control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24

round(quantile(mu2, probs = pro,control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24

round(quantile(mu3, probs = pro,control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24

round(quantile(mu4, probs = pro,control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24



# ----------
round(sd.circular(mu1, control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24

round(sd.circular(mu2, control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24

round(sd.circular(mu3, control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24

round(sd.circular(mu4, control.circular = list(modulo="2pi")) * (180/pi), 3) / 360 * 24

