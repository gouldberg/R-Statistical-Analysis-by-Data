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
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

# here na.omit is required
cod.fitp <- cbind(na.omit(CodParasites), intensity_pred = predict(modp, type = "response"))


head(cod.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(cod.fitp, aes(x = length, y = intensity_pred)) + theme_bw() + geom_line(size = 2.5, aes(color = year)) + geom_point(colour = "black", size = 1.5)

gg + facet_grid(~ area)

