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
# Distplot
#
# Hoaglin and Tukey (1985) developed similar plots of a count metameter against k, which appear as a straight line
# when a data distribution follows the general power series family of discrete distribution
#
# For the Binomial distribution, a "binomialness" plot is constructed by plotting log(n(k) / N(n,k)) against k.
# If the points in this plot approximate a stright line, the slope is interpreted as log(p / (1-p)),
# so the binomial parameter p may be estimated as p = exp(b) / ( 1 + exp(b) )
# ------------------------------------------------------------------------------

distplot(data_home, type = "poisson", xlab = "Home Goals")



# AwayGoals lacks fit at AwayGoals = 0 - 1
distplot(data_away, type = "poisson", xlab = "Away Goals")



# TotalGoals lacks fit at TotalGoals = 0 - 2
distplot(data_total, type = "poisson", xlab = "Total Goals")
