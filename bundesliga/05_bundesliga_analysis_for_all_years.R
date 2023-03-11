setwd("//media//kswada//MyFiles//R//bundesliga")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bundesliga
# ------------------------------------------------------------------------------
data("Bundesliga", package = "vcd")

data <- Bundesliga



# ------------------------------------------------------------------------------
# Calculate the mean and variance of these variables
# ------------------------------------------------------------------------------
# This time using the data for all years in which there was the standard number (306) of games, that is for Year > 1965
data_df <- data %>% filter(Year > 1965) %>% dplyr::select(HomeGoals, AwayGoals) %>% mutate(TotalGoals = HomeGoals + AwayGoals)
head(data_df)


apply(data_df, MARGIN=2, FUN=function(x) c(mean = mean(x), var = var(x)))


# mean of HomeGoals is larger than that of AwayGoals
# variance is larger for all HomeGoals, AwayGoals and TotalGoals



# ------------------------------------------------------------------------------
# Fit Poisson distribution to all of HomeGoals, AwayGoals and TotalGoals
# ------------------------------------------------------------------------------
data_home <- data %>% dplyr::select(HomeGoals) %>% table()
data_away <- data %>% dplyr::select(AwayGoals) %>% table()
data_total <- data %>% mutate(TotalGoals = HomeGoals + AwayGoals) %>% dplyr::select(TotalGoals) %>% table()


data_home_fit <- vcd::goodfit(data_home, type = "poisson")
data_away_fit <- vcd::goodfit(data_away, type = "poisson")
data_total_fit <- vcd::goodfit(data_total, type = "poisson")


# ----------
# It seems that all lacks fitting
summary(data_home_fit)
summary(data_away_fit)
summary(data_total_fit)



# ------------------------------------------------------------------------------
# Diagnosing by rootogram
# ------------------------------------------------------------------------------
plot(data_home_fit, type="hanging", shade=TRUE)
plot(data_home_fit, type="deviation", shade=TRUE)


plot(data_away_fit, type="hanging", shade=TRUE)
plot(data_away_fit, type="deviation", shade=TRUE)


plot(data_total_fit, type="hanging", shade=TRUE)
plot(data_total_fit, type="deviation", shade=TRUE)



# ------------------------------------------------------------------------------
# Distplot
# ------------------------------------------------------------------------------
distplot(data_home, type = "poisson", xlab = "Home Goals")

distplot(data_away, type = "poisson", xlab = "Away Goals")

distplot(data_total, type = "poisson", xlab = "Total Goals")
