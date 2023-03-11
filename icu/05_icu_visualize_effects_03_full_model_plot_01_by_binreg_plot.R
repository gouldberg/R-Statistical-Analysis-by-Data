setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ----------
levels(ICU$cancer) <- c("-", "Cancer")
levels(ICU$admit) <- c("-", "Emerg")
levels(ICU$uncons) <- c("-", "Uncons")


icu.glm2 <- glm(died ~ age + cancer + admit + uncons, data = ICU2, family = binomial)



# ------------------------------------------------------------------------------
# Full-model plots
#
# Disply of fitted values for a binary regression model with one numeric predictor, conditioned by zero or many co-factors
# Check baseline (no risk factors) and risk factors impact
# ------------------------------------------------------------------------------

# Fitted log odds of death in ICU with 1 standard error confidence bands
# by default, binreq_plot() uses the first numeric predictors as the horizontal variable
vcd::binreg_plot(icu.glm2, type = "link", conf_level = 0.68,
                 legend = FALSE, labels = TRUE, labels_just = c("right", "bottom"),
                 cex = 0, point_size = 0.8, pch = 15:17,
                 ylab = "Log odds (died)",  ylim = c(-7, 4))


# --> 
# It is apparement that the log odds of mortality increases with age in all cases.
# Relative to the line labeled "-:-:-" (no risk factors), mortality is higher when any of these risk factors are present,
# particularly when the patient is admitted to emergency.
# It is highest when the patient is also unconscious at admission.
# The vertical gaps between lines that share a common risk indicate the additional increment from one more risk.

# The plotted points show the number and age distribution of these various combinations.
# The greatest number of patients have only Emerg as a risk factor and only one patient was unconscious with no other risk.



# ----------
# Fitted probaility of death in ICU
vcd::binreg_plot(icu.glm2, type = "response", conf_level = 0.68,
                 legend = FALSE, labels = TRUE, labels_just = c("right", "bottom"),
                 cex = 0, point_size = 0.8, pch = 15:17,
                 ylab = "Probability (died)",  ylim = c(0, 1))



# ----------
# subsetting
vcd::binreg_plot(icu.glm2, type = "link", subset = admit == "Emerg", main = "Emerg",
                 conf_level = 0.68,
                 legend = FALSE, labels = TRUE, labels_just = c("right", "bottom"),
                 cex = 0, point_size = 0.8, pch = 15:17,
                 ylab = "Log odds (died)",  ylim = c(-7, 4))



