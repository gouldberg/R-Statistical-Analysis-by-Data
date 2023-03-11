setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data


# ----------
# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# plot residuals by mosaic plot
# ------------------------------------------------------------------------------


vnames <- list(set_varnames = c(B = "Breathlessness", W = "Wheeze"))


lnames <- list(B = c("B", "b"), W = c("W", "w"))


mosaic(cm.glm1, ~ Age + B + W, labeling_args = vnames, set_labels = lnames)



