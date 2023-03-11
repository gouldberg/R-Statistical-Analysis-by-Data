setwd("//media//kswada//MyFiles//R//djia")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Dow Jones Industrial Average
#   - Dow Jones Industrial Average (DJIA) stock market index in 2013
#   - There are 252 trading days in 2013. The 252 by 30 matrix data contains the daily closing prices of the 30 stocks.
# ------------------------------------------------------------------------------

djia <- read.csv("Dow_companies_data.csv")


str(djia)


head(djia)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

# Plot cumulative return of the stock value of Exxon-Mobil, ticker symbol XOM.

( tmp <- 100 * ( djia[,"XOM"] - djia[1,"XOM"] ) / djia[1,"XOM"] )

par(mfrow=c(2,1))
plot(djia[,"XOM"] ~ djia[,"Date"])
plot(tmp ~ djia[,"Date"])



