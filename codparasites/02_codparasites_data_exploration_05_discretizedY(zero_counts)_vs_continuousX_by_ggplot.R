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
# data exploration:  discretized Y (zero counts probability) vs. continuous X by ggplot
# ------------------------------------------------------------------------------


library(ggplot2)


# Convert prevalence to numeric
CodParasites$prevalence2 <- ifelse(CodParasites$intensity == 0, 0, 1)

ggplot(CodParasites, aes(x = length, y = as.numeric(prevalence2) - 1)) + geom_jitter(position = position_jitter(height = 0.05), alpha = 0.25) +
  geom_rug(position = "jitter", sides = "b") +
  stat_smooth(method = "loess", color = "red", fill = "red", size = 1.5) + labs(y = "prevalence")



# -->
# The loess smoothed curve shows an apparent U-shaped relationship,
# however, the plotted observations and the confidence bands make clear that there is very little data in the extremes of length.
