setwd("//media//kswada//MyFiles//R//trd")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  trd (Tokyo rainfall data)
# ------------------------------------------------------------------------------

data("trd", package = "gamlss.data")


str(trd)



# ------------------------------------------------------------------------------
# data exploration:  define binary response variable and time
# ------------------------------------------------------------------------------

# Define the binomial response variable
# making sure that 29 Feb was observed ony once
NY <- 2 - trd

NY[60] <- 1 - trd[60]


y <- cbind(trd, NY)



# variable for the different days of the year
ti <- 1:366



# ----------
# Note that y is rain days and non-rain days (2 column)
unique(y)



# ----------
par(mfrow = c(2, 1))
plot(trd, type = "l", ylab = "", main = "Rain days", las = 1)
plot(y[,"NY"], type = "l", ylab = "", main = "No-Rain days", las = 1)




