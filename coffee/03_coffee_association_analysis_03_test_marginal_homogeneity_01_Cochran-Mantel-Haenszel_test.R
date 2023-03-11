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
# Test marginal homogeneity by Cochran-Mantel-Haenszel Test
# ------------------------------------------------------------------------------

# Null that two nominal variables are conditionally independent in each stratum, assuming that there is no 3-way interaction
# z: a factor object with at least 2 levels identifying to which stratum the corresponding elements in x and y belong
with(Coffee, mantelhaen.test(x = purchase, y = brand, z = person))



# -->
# rejecting Null



