setwd("//media//kswada//MyFiles//R//cigar")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cigar
# ------------------------------------------------------------------------------

data("Cigar", package = "Ecdat")

dim(Cigar)

str(Cigar)


car::some(Cigar)


# ----------
# states To be excluded since tax for cigaret is increased by 50 cents or more from 1988.
# If those states are included, the assumption of Common Trend does not hold.
# Alaska, Hawaii, Maryland, Michigan, New Jersey, New York, Washington
skip_state <- c(3, 9, 10, 22, 21, 23, 31, 33, 48)


Cigar <- Cigar %>% filter(!state %in% skip_state, year >= 70) %>% mutate(area = if_else(state == 5, "CA", "Rest of US"))



# ------------------------------------------------------------------------------
# data preparation
# ------------------------------------------------------------------------------

library(tidyverse)


# only sales of California
Y <- Cigar %>% filter(state == 5) %>% pull(sales)


# sales of other US states as covariates
X_sales <- Cigar %>% filter(state != 5) %>% select(state, sales, year) %>% spread(state, sales)



# ----------
pre_period <- c(1:NROW(X_sales))[X_sales$year < 88]

post_period <- c(1:NROW(X_sales))[X_sales$year >= 88]



# ----------
CI_data <- cbind(Y, X_sales) %>% select(-year)



# ------------------------------------------------------------------------------
# CausalImpact analysis
# ------------------------------------------------------------------------------

impact <- CausalImpact::CausalImpact(CI_data, 
                                     pre.period = c(min(pre_period), max(pre_period)),
                                     post.period = c(min(post_period), max(post_period)))


plot(impact)



# ----------
impact


impact$series

impact$summary

impact$report

impact$model
