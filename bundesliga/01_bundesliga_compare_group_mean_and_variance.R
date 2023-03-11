setwd("//media//kswada//MyFiles//R//bundesliga")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bundesliga
# ------------------------------------------------------------------------------
data("Bundesliga", package = "vcd")

data <- Bundesliga

data

str(data)



# ------------------------------------------------------------------------------
# Convert to table
# ------------------------------------------------------------------------------
BL1995 <- xtabs(~ HomeGoals + AwayGoals, data = Bundesliga, subset = (Year == 1995))

BL1995



# ------------------------------------------------------------------------------
# Convert table to dataframe
# ------------------------------------------------------------------------------
data_df <- as.data.frame(BL1995, stringsAsFactors = FALSE)

data_df <- within(data_df, {
  HomeGoals <- as.numeric(HomeGoals)
  AwayGoals <- as.numeric(AwayGoals)
  TotalGoals <- HomeGoals + AwayGoals
})

str(data_df)

head(data_df)



# ------------------------------------------------------------------------------
# Calculate the mean and variance of these variables
# ------------------------------------------------------------------------------
# expand to ungrouped form
data_df2 <- expand.dft(data_df)
head(data_df2)


apply(data_df2, MARGIN=2, FUN=function(x) c(mean = mean(x), var = var(x)))


# -->
# mean of home goals are larger than away goals
# mean and variance is close all for HomeGoals, AwayGoals, and TotalGoals


