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
# data exploration:  log Y by category
# ------------------------------------------------------------------------------

lattice::bwplot(intensity ~ year | area, data = CodParasites)

lattice::bwplot(log(intensity + 1) ~ year | area, data = CodParasites)


# -->
# median of year 2000 is largest at least 3 out of 4 areas (except mageroya)




# ------------------------------------------------------------------------------
# data exploration:  log Y (only positive values) by category
# ------------------------------------------------------------------------------

library(ggplot2)


CodParasitesPos <- subset(CodParasites, prevalence == "yes")


ggplot(CodParasitesPos, aes(x = year, y = intensity)) + geom_boxplot(outlier.size = 3, notch = TRUE, aes(fill = year), alpha = 0.3) +
         geom_jitter(position = position_jitter(width= 0.1), alpha = 0.25) +
         facet_grid(. ~ area) + 
         scale_y_log10(breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
         theme(legend.position = "none") + labs(y = "intensity (log scale)")



# -->
# Most of these distributions are positively skewed and there are a few high outliers, but probably not more than would be expected in a sample of this size.
# The positive counts (degree of infection) are also higher in all years in Varangerfjord than other areas.
# You can ses also see that the intensity values were generally lower in 2001 than other years.
