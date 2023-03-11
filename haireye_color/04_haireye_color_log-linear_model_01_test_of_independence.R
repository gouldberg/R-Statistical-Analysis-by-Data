setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

data <- HairEyeColor

data


# collapse table to 2-way table (Hair(1) and Eye(2))
( haireye <- margin.table(data, 1:2) )



# ------------------------------------------------------------------------------
# 3-way and larger tables
# Mosaic plot
# ------------------------------------------------------------------------------
HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"),]
vcd::mosaic(HEC, rot_labels = c(right = -45))


# --> There is no systematic association between sex and the combinations of hair and eye color.
# The proportion of male-female students is roughly the same in almost all hair/eye color groups.
# Yet, among blue-eyed blonds, there seems to be an overabundance of females, and the proportion of blue-eyed males with brown in hair also looks suspicious.



# ------------------------------------------------------------------------------
# 3-way and larger tables:  classical test of independence
# First, the independence of Hair and Eye is equivalent to the model [Hair][Eye]
# ------------------------------------------------------------------------------
HE <- MASS::loglm(~ Hair + Eye, data = haireye)



# The output includes both the chi-square statistic and the deviance test statistics, 
# both significant, indicating strong lack of fit.
HE



# ------------------------------------------------------------------------------
# 3-way and larger tables:  baseline models
# From mosaic plot, the hypothesis is that Hair and Eye are jointly independent of Sex.
# To test formally, we fit corresponding model [HairEye][Sex]
# ------------------------------------------------------------------------------
HE_S <- MASS::loglm(~ Hair * Eye + Sex, data = HairEyeColor)
HE_S


#  --> non-significant Pearson chi-square (15) = 19.567, p = 0.189


# ----------
# residuals
residuals(HE_S, type = "pearson")



# ----------
# Show residuals on mosaic plot
# vcd::mosaic(expected = ~, labeling = labeling_residuals) shows residuals from loglm model on mosaic plot
HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"),]
vcd::mosaic(HEC, expected = ~ Hair * Eye + Sex, labeling = labeling_residuals, digits = 2, rot_labels = c(right = -45))



# --> Although non-significant, the two largest residuals highlighted in the plot account for nearly half (-2.15^2 + 2.03^2 = 8.74) of the lack of fit,
# and so are worthy of attention here.

