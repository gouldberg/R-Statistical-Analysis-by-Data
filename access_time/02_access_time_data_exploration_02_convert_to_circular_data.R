# setwd("//media//kswada//MyFiles//R//access_time//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//access_time//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  access time
# ------------------------------------------------------------------------------

data <- read.csv("access_time.csv", header = T)


str(data)




# ------------------------------------------------------------------------------
# data exploration:  convert to circular data
# ------------------------------------------------------------------------------

library(circular)


checkin_data <- as.circular(data,
                          units = "hours", control.circular = list(modulo="2pi"))

head(data)

head(checkin_data)



# ----------
par(mfrow = c(1,2))

hist(data$x, breaks = seq(0, 24, by = 0.25))


# note that 0.5pi --> 0 hour
plot(checkin_data, stack = TRUE, bins = 6 * 24, col = "darkgray")



# ----------
summary(checkin_data)




# ------------------------------------------------------------------------------
# data exploration:  convert to radian:  if 24:00:00 --> 2 * pi
# ------------------------------------------------------------------------------

x <- checkin_data

for (i in 1:length(checkin_data)){
  x[i] <- as.vector((checkin_data[i]/24) * (2*pi))
}


head(x)


summary(x)


