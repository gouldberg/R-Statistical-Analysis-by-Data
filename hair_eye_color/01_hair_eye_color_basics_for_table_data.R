
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hair Eye Color
# ------------------------------------------------------------------------------

data(HairEyeColor, package = "datasets")


str(HairEyeColor)


HairEyeColor




# ------------------------------------------------------------------------------
# struc table
# ------------------------------------------------------------------------------


library(vcd)


hec <- structable(Eye ~ Sex + Hair, data = HairEyeColor)


hec


hec["Male",]




# ----------
structable(Hair + Sex ~ Eye, HairEyeColor)




# ------------------------------------------------------------------------------
# collapse table
# ------------------------------------------------------------------------------

( haireye <- margin.table(HairEyeColor, 1:2) )




# ------------------------------------------------------------------------------
# mosaic display
# ------------------------------------------------------------------------------

library(vcd)


mosaic(haireye, labeling = labeling_values)




# ------------------------------------------------------------------------------
# proportion, table margins
# ------------------------------------------------------------------------------

# proportion

( hair <- margin.table(haireye, 1) )


prop.table(hair)



# ----------
# add margin to table  (margin to row)

round(addmargins(prop.table(haireye, 1), 2), 3)


# add margin to table  (margin to column)

round(addmargins(prop.table(haireye, 2), 1), 3)


# add margin to table  (margin to row and column)

round(addmargins(prop.table(haireye)), 3)




# ------------------------------------------------------------------------------
# table data, data frame
# ------------------------------------------------------------------------------

library(dplyr)
library(vcdExtra)


# table to case form
dat <- expand.dft(HairEyeColor)


dat$id <- 1:nrow(dat)


head(dat)



# ----------
# case form to table

tab <- xtabs(~ Hair + Eye + Sex, data = dat)

tab


HairEyeColor

