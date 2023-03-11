setwd("//media//kswada//MyFiles//R//bundesliga")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bundesliga
# ------------------------------------------------------------------------------
data("Bundesliga", package = "vcd")

data <- Bundesliga

BL1995 <- data %>% filter(Year == 1995)

data_home <- BL1995 %>% dplyr::select(HomeGoals) %>% table()
data_away <- BL1995 %>% dplyr::select(AwayGoals) %>% table()
data_total <- BL1995 %>% mutate(TotalGoals = HomeGoals + AwayGoals) %>% dplyr::select(TotalGoals) %>% table()



# ------------------------------------------------------------------------------
# Use goodfit() to fit the posisson distribution
# ------------------------------------------------------------------------------
data_fit_home <- vcd::goodfit(data_home, type = "poisson")
data_fit_away <- vcd::goodfit(data_away, type = "poisson")
data_fit_total <- vcd::goodfit(data_total, type = "poisson")



# ------------------------------------------------------------------------------
# Rootogram
#
# plot.goodfit() method for "goodfit" objects
# ------------------------------------------------------------------------------
plot(data_fit_home, type = "hanging", shade = TRUE)
plot(data_fit_away, type = "hanging", shade = TRUE)
plot(data_fit_total, type = "hanging", shade = TRUE)


# TotalGoals lack of fit to Poisson distribution
plot(data_fit_home, type = "deviation", shade = TRUE)
plot(data_fit_away, type = "deviation", shade = TRUE)
plot(data_fit_total, type = "deviation", shade = TRUE)





