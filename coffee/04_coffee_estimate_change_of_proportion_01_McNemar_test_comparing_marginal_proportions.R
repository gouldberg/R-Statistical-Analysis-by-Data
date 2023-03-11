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




# ------------------------------------------------------------------------------
# Data preparation:  HighPoints and Others
# ------------------------------------------------------------------------------

from <- c(1, 2, 3, 4, 5)

to <- c("X01_HighPoint", "X02_Others", "X02_Others", "X02_Others", "X02_Others")

Coffee$brand <- to[match(Coffee$y, from)]



# ----------
library(tidyverse)
tmp <- Coffee %>% dplyr::select(person, purchase, brand) %>% spread(., key = purchase, value = brand)
# 0: second purchase  1: first purchase
colnames(tmp) <- c("person", "second_p", "first_p")
cof.tab <- xtabs(~ first_p + second_p, data = tmp)


cof.tab




# ------------------------------------------------------------------------------
# McNemar Test Comparing Marginal Proportions
# ------------------------------------------------------------------------------

# Do not use continuity correction
mcnemar.test(cof.tab, correct = FALSE)


# -->
# The test rejects marginal homogeneity,
# indicating HighPoints increaed in second purchase









