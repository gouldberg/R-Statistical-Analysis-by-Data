# setwd("//media//kswada//MyFiles//R//google_trend//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//google_trend//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  google trend
# ------------------------------------------------------------------------------

library(tidyverse)

s.dfData2 <- read_csv("dfData2.csv", locale = locale(encoding = "cp932"))


str(s.dfData2)


dim(s.dfData2)


car::some(s.dfData2)



# ------------------------------------------------------------------------------
# 動的因子分析のための整形
# ------------------------------------------------------------------------------

mgData <- s.dfData2 %>%
  left_join(s.dfTable2 %>% dplyr::select(nCityID, nSum, gSD2), by = "nCityID") %>%
  filter(nSum > 10000, gSD2 > 11.75) %>%
  dplyr::select(date, nCityID, hits) %>%
  spread(nCityID, hits) %>%
  dplyr::select(-date) %>%
  as.matrix(.)


mgCov <- tibble(
  gC1 = sin(2*pi*1*(1:nrow(mgData))/52.14286), 
  gC2 = sin(2*pi*2*(1:nrow(mgData))/52.14286), 
  gC3 = sin(2*pi*3*(1:nrow(mgData))/52.14286), 
  gC4 = sin(2*pi*4*(1:nrow(mgData))/52.14286), 
  gC5 = cos(2*pi*1*(1:nrow(mgData))/52.14286), 
  gC6 = cos(2*pi*2*(1:nrow(mgData))/52.14286), 
  gC7 = cos(2*pi*3*(1:nrow(mgData))/52.14286), 
  gC8 = cos(2*pi*4*(1:nrow(mgData))/52.14286)
) %>%
  as.matrix(.)


s.lDataDFA100 <- list(
  mgData = mgData, 
  mgCov  = mgCov
)



head(s.lDataDFA100$mgData)

head(s.lDataDFA100$mgCov)

