setwd("//media//kswada//MyFiles//R//vouchers")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  vauchers
#   - data from Angrist et al. (2002) "Vouchers for private schoolin in Colombia: Dvidence from a randomized natural experiment".
#   - Covariates
#       - SVY:  survey conducted by telephone ?
#       - HSVISIT:  survey conducted by face-to-face
#       - DMONTH1-12:  survey conducted in which month ?
#       - AGE:  age of student at the timing when survey is conducted
#       - SEX:  sex of the student
#       - STRATA1-6, MS:  social status classification of the parents of the surveyed student
# ------------------------------------------------------------------------------

vouchers <- read_tsv("vouchers.txt")

str(vouchers)

dim(vouchers)


car::some(vouchers)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
