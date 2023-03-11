setwd("//media//kswada//MyFiles//R//vouchers")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  vauchers
# ------------------------------------------------------------------------------

vouchers <- read_tsv("vouchers.txt")

str(vouchers)

dim(vouchers)


car::some(vouchers)



# ------------------------------------------------------------------------------
# data exploration:  data distribution by each variable
# ------------------------------------------------------------------------------

Hmisc::describe(vouchers)



# ------------------------------------------------------------------------------
# data exploration:  mosaic matrices
# ------------------------------------------------------------------------------

tab <- xtabs(~ SVY + HSVISIT + SEX + VOUCH0, data = vouchers)

tab

dim(tab)



# ----------
library(vcd);  library(vcdExtra);


pairs(tab, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))

