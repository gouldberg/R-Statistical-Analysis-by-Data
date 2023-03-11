setwd("//media//kswada//MyFiles//R//privacy")

packages <- c("dplyr", "MPsychoR", "corrplot", "BayesFM")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Privacy
#   - data from Treiblmainer (2006, 2011), contains items that measure various advantages and disadvantages online users
#     perceive when providing personal information on the Internet.
#   - The items are based on 25 qualitative interviews and represent opinions of both organizations and individuals.
#     Advantages of prociding personal information online include support for purchasing decisions, increased satisfaction, targeted communication,
#     participation in raffles, time savings, and interesting content.
#     Disadvantages include unsolicited advertising, excessive data collection, lack of information about data usage,
#     and decreasing service quality.
#   - We consider six items relating to advantages of personal communication (variable names starting with apc) and four items related to disadvantages of
#     personal communication (those starting with dpc in the variable names). 
#     Each item was measured using a slide bar (from 1 to 100).
# ------------------------------------------------------------------------------

data("Privacy", package = "MPsychoR")


str(Privacy)



# ------------------------------------------------------------------------------
# basic analysis:  correlation among variables
# ------------------------------------------------------------------------------


Privstd <- scale(Privacy)

head(Privstd)



# ----------
# Entire correlation matrix
corrplot(cor(Privstd))



# -->
# The plot shows two separate sets of variables (advantage items vs. disadvantage items) with high correlations among the items within each set.
# Byhaving a closer look, we see that the advantage itemset could potentially be split up into another two subsets.



# ------------------------------------------------------------------------------
# basic analysis:  Check the linearity among the variables
# ------------------------------------------------------------------------------

car::scatterplotMatrix(Privstd)
