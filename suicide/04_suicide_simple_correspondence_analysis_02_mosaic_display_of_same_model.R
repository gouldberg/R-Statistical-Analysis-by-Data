setwd("//media//kswada//MyFiles//R//suicide")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Suicide rates in Germany
# ------------------------------------------------------------------------------
data("Suicide", package = "vcd")

data <- Suicide

data




# ------------------------------------------------------------------------------
# Visualize pattern of associations by mosaic plot
#  - construct the mosaic display for the same model analyzed by correspondence analysis
# ------------------------------------------------------------------------------

tab2 <- xtabs(Freq ~ sex + age.group + method2, data = data)

tab2


# reorder methods by CA socres on Dim 1
suicide.ca$colcoord
method.order <- order(suicide.ca$colcoord[,1])
method.order


( tab2 <- tab2[,,method.order] )


# delete other
tab2 <- tab2[,,-5]


ftable(tab2)




# ----------
# Use the argument expected = ~ age.group * sex + method2 to supply the model formula
# Note that this formula is for correspondence analysis, different from xtabs formula above (see "tab2")
mosaic(tab2, expected = ~ age.group * sex + method2, shade = TRUE, legend = FALSE,
       labeling_args = list(abbreviate_labs = c(FALSE, FALSE, 5)), rot_labels = c(0, 0, 0, 90))



# -->
# The plot shows the prevalence of gun adn gas among younger males and decreasing with age,
# whereas use of hang increases with age.
# For females, these 3 methods are used less frequently, whereas poison, jump, and drown occur more often.
# For females, the excess prevalence of these high-frequency methods varies somewhat less with age than it does for males


