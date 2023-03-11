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
# Estimate difference and 95% interval of the difference
# ------------------------------------------------------------------------------

library(PropCIs)



cof.tab


prop.table(cof.tab)


diffpropci.Wald.mp(42, 78, sum(cof.tab), conf.level = 0.95)


# The proporion of HighPoint is decreased by
171/541 - 135/541



# -->
# The estiamted change (difference) = 0.067
# and 95% confidence interval is 0.027 - 0.106

# The small p-value for the overall test of marginal homogeneity mainly reflects a decrease in the proportion choosing HighPoint and 
# an increase in the proportion choosing Sanka, with no evidence of change for the other brands

