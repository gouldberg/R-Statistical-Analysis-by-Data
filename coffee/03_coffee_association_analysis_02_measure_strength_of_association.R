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
# Measure strength of association:  proportion from first buy to second buy
# ------------------------------------------------------------------------------

round(100 * prop.table(cof.tab, margin=1), digits = 2)





# ------------------------------------------------------------------------------
# Measure strength of association:  X^2 tests and Cramer's V
# ------------------------------------------------------------------------------

assocstats(cof.tab)



# -->
# X^2 tests and measures of association (normalized variants of the X^2 statistic)
# Caution is needed for interpretation since the maximum values depend on the table dimension.
# Cramer's V = 0.463



# ------------------------------------------------------------------------------
# Measures of association:  CMH test
# ------------------------------------------------------------------------------

vcdExtra::CMHtest(t(cof.tab))




