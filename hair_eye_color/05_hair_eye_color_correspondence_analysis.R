
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hair Eye Color
# ------------------------------------------------------------------------------

data(HairEyeColor, package = "datasets")


str(HairEyeColor)


HairEyeColor




# ------------------------------------------------------------------------------
# correspondence analysis
# ------------------------------------------------------------------------------

haireye


library(ca)


# only available for 2 dimension table

haireye.ca <- ca(haireye)


summary(haireye.ca)


plot(haireye.ca)

