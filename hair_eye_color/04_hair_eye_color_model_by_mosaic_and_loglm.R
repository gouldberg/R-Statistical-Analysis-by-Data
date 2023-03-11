
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hair Eye Color
# ------------------------------------------------------------------------------

data(HairEyeColor, package = "datasets")


str(HairEyeColor)


HairEyeColor




# ------------------------------------------------------------------------------
# model by mosaic
# ------------------------------------------------------------------------------

HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"), ]


# mutual
mosaic(margin.table(HEC, 1:2), expected = ~ Hair + Eye, labeling = labeling_residuals, digits = 2, rot_labels = c(right = -45))


# conditional
mosaic(HEC, expected = ~ Hair * Sex + Eye * Sex, labeling = labeling_residuals, digits = 2, rot_labels = c(right = -45))


# joint
mosaic(HEC, expected = ~ Hair * Eye + Sex, labeling = labeling_residuals, digits = 2, rot_labels = c(right = -45))




# ------------------------------------------------------------------------------
# model by loglm
# ------------------------------------------------------------------------------

library(MASS)


# mutual
mod_m <- loglm(~ Hair + Eye + Sex, data = HEC)


# conditional
mod_c <- loglm(~ Hair * Sex + Eye * Sex, data = HEC)


# joint
mod_j <- loglm(~ Hair * Eye + Sex, data = HEC)



# ----------
LRstats(mod_m, mod_c, mod_j)



# ----------
loglin2formula(joint(1, table = HEC))
loglin2formula(joint(2, table = HEC))
loglin2formula(joint(3, table = HEC))

HEC.mods <- seq_loglm(HEC, type = "joint")

LRstats(HEC.mods)

