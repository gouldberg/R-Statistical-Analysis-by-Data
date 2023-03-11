# setwd("//media//kswada//MyFiles//R//coffee")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//coffee")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coffee
# ------------------------------------------------------------------------------

Coffee <- read.table(file = "Coffee.dat", header = T, stringsAsFactors = FALSE)


str(Coffee)


car::some(Coffee)



# ----------
from <- c(1, 2, 3, 4, 5)
to <- c("X01_HighPoint", "X02_TastersChoice", "X03_Sanka", "X04_Nescafe", "X05_Brim")
Coffee$brand <- to[match(Coffee$y, from)]

library(tidyverse)
tmp <- Coffee %>% dplyr::select(person, purchase, brand) %>% spread(., key = purchase, value = brand)
# 0: second purchase  1: first purchase
colnames(tmp) <- c("person", "second_p", "first_p")
cof.tab <- xtabs(~ first_p + second_p, data = tmp)


cof.tab




# ------------------------------------------------------------------------------
# Test marginal homogeneity
#   - H0: beta1 = beta2 = beta3 = beta4 = 0
#       log(P(1st purchase = TestersChoice) / P(1st purchase = HighPpoint)) = alpha1 + beta1 = alpha(TestersChoice) + beta(TestersChoice)
#       log(P(1st purchase = TestersChoice) / P(1st purchase = HighPpoint)) = alpha1 + beta1 = alpha(TestersChoice) + beta(TestersChoice)
#
#   - Using GEE methodology, fit baseline-category logit model
# ------------------------------------------------------------------------------

library(multgee)


fit <- nomLORgee(y ~ purchase, id = person, LORstr = "independence", data = Coffee)


summary(fit)



# -->
# beta1 = 0.323 is the marginal log odds for the 2 * 2 table with elements (171, 55) from row totals 1 and 5 and (135, 60) from column totals 1 and 5
# To Test H0: beta1 = beta2 = beta3 = beta4  =0



# ------------------------------------------------------------------------------
# To Test H0: beta1 = beta2 = beta3 = beta4 = 0
# Wald Test of Nested GEE models
# ------------------------------------------------------------------------------

fit0 <- nomLORgee(y ~ 1, id = person, LORstr = "independence", data = Coffee)


waldts(fit0, fit)



# -->
# The Wald statistic comparing the models is 12.49, based on df = 4, which provides evidence of marginal heterogeneity



