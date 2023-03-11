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
# masaic plot by shading_Friendly
#  - An overall test for association using Pearson' X^2 may not be significant
# ------------------------------------------------------------------------------

# margin() to plot is larger a little bit

library(vcd)

mosaic(cof.tab, gp = shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digits = 2)



# -->
# Some brands are more suffering from no second purchase



# ----------
chisq.test(cof.tab)




# ------------------------------------------------------------------------------
# masaic plot by shaindg_max
#  - the maximum residual test may highlight one or more cells worthy of greater attension
# ------------------------------------------------------------------------------

# shading levels determined by significant maximum residuals via shading_max
# This shows that significant deviations from independence occur in the cour corner cells, corresponding to more of the 
# treated group showing marked improvement, and more of the placebo group showing no improvement

mosaic(cof.tab, gp = shading_max, margin = c(right = 1))




# ----------
# how shading_max function:

# Pearson residuals for this table
residuals(chisq.test(cof.tab))



# The shading_max() function then calls coindep_test() to generate n = 1000 random tables with the same margins,
# and computes the maximum residual statistic for each
# This gives a non-parametric p-value for the test of independence
set.seed(1234)

cof.tab_max <- coindep_test(cof.tab)

cof.tab_max



# Finally, the 0.90 and 0.99 quantiles of the simulation distribution are used as shading levels,
# passed as the value of the interpolate argument

cof.tab_max$qdist(c(0.90, 0.99))




