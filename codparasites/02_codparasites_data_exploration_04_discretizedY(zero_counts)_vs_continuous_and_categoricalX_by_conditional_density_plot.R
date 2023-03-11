setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")


# ------------------------------------------------------------------------------
# data exploration:  discretized Y (zero counts) vs. continuous and categorical X
# ------------------------------------------------------------------------------

data <- na.omit(CodParasites)

plot(prevalence ~ factor(area), data = data, ylab = "Prevalence", ylevels=2:1)

plot(prevalence ~ factor(year), data = data, ylab = "Prevalence", ylevels=2:1)

plot(prevalence ~ factor(stage), data = data, ylab = "Prevalence", ylevels=2:1)

plot(prevalence ~ factor(sex), data = data, ylab = "Prevalence", ylevels=2:1)

plot(prevalence ~ length, data = data, breaks = quantile(length, probs = seq(0,1,0.2)), ylevels=2:1, ylab = "Prevalence")

plot(prevalence ~ weight, data = data, breaks = quantile(weight, probs = seq(0,1,0.2)), ylevels=2:1, ylab = "Prevalence")



# ----------
# similar plot by conditional density plot
# --> no need to distretize continuous variables

cdplot(prevalence ~ length, data = data, ylab = "Prevalence", ylevels=2:1)

cdplot(prevalence ~ weight, data = data, ylab = "Prevalence", ylevels=2:1)




# ------------------------------------------------------------------------------
# data exploration:  discretized Y (zero counts) vs. categorical X by doubledecker
# ------------------------------------------------------------------------------

cp.tab <- xtabs(~ area + year + factor(is.na(prevalence) | prevalence == "yes"), data = CodParasites)



dimnames(cp.tab)[3] <- list(c("No", "Yes"))

names(dimnames(cp.tab))[3] <- "prevalence"


doubledecker(prevalence ~ area + year, data = cp.tab, margins = c(1, 5, 3, 1))



# ----------
# mosaic plot in the doubledecker format
doubledecker(prevalence ~ area + year, data = cp.tab,
             gp = shading_hcl, expected = ~ year:area + prevalence,
             margins = c(1, 5, 3, 1))


# -->
# Shading reflects departure from a model in which prevalence is independent of are and year jointly.
# This plot shows further that prevlence differs substantially over the area-year combinations, 
# We would expect an interaction in the model for zero counts.


# Varangerfjord stands out as having consistently greater prevalence in all years tha nexpected under this model.
