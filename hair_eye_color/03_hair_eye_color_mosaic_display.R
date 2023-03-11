
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hair Eye Color
# ------------------------------------------------------------------------------

data(HairEyeColor, package = "datasets")


str(HairEyeColor)


HairEyeColor




# ------------------------------------------------------------------------------
# mosaic plot:  shading
# ------------------------------------------------------------------------------

# reorder eye colors from dark to light


haireye2 <- as.table(haireye[, c("Brown", "Hazel", "Green", "Blue")])


mosaic(haireye2, shade = TRUE)



# more shading levels
mosaic(haireye2, shade = TRUE, gp_args = list(interpolate = 1:4))


mosaic(HairEyeColor, shade = TRUE, gp_args = list(interpolate = 1:4))




# ----------
# suppress = 0:  show even small values
mosaic(haireye2, gp = shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digis = 2)


mosaic(haireye2, gp = shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digis = 2)




# ------------------------------------------------------------------------------
# generalized mosaic matrices and pairs plots
# ------------------------------------------------------------------------------

library(gpairs)


# use data frame (not table)

gpairs(dat[,c(1,2,3)], diag.pars = list(fontsize = 20),
       mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate = 1:4)))


