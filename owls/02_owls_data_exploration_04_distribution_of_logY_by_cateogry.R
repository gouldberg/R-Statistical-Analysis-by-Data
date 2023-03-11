# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ------------------------------------------------------------------------------
# data exploration:  log Y by category
# ------------------------------------------------------------------------------

# We apply log10(Y+1) transformation, which was also used in Roulin and Bersier (2007)

boxplot(log10(NegPerChick + 1) ~ Nest, data = Owls, varwidth = TRUE)

boxplot(log10(NegPerChick + 1) ~ SexParent, data = Owls, varwidth = TRUE)

boxplot(log10(NegPerChick + 1) ~ FoodTreatment, data = Owls, varwidth = TRUE)

boxplot(log10(NegPerChick + 1) ~ FoodTreatment + SexParent, data = Owls, varwidth = TRUE)

boxplot(log10(NegPerChick + 1) ~ BroodSize, data = Owls, varwidth = TRUE)




# ------------------------------------------------------------------------------
# data exploration:  log Y (only positive values) by category
# ------------------------------------------------------------------------------

library(ggplot2)


ggplot(Owls, aes(x = Nest, y = NegPerChick + 0.1)) + geom_boxplot(outlier.size = 3, notch = TRUE, aes(fill = Nest), alpha = 0.3) +
#         geom_jitter(position = position_jitter(width= 0.1), alpha = 0.25) +
#         facet_grid(. ~ SexParent) + 
         scale_y_log10(breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
         theme(legend.position = "none") + labs(y = "NegPerChick + 0.1 (log scale)")


