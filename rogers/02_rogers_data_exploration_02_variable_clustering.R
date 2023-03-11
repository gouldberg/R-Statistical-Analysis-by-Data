setwd("//media//kswada//MyFiles//R//rogers")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rogers
# ------------------------------------------------------------------------------

data("Rogers", package = "MPsychoR")

str(Rogers)

dim(Rogers)

car::some(Rogers)



# ------------------------------------------------------------------------------
# data exploration:  Variable Clustering
#
#   - Hierarchical cluster analysis on an similatrity matrix (such as squared correlations)
#   - It is often advisable to use robust (e.g., rank-based) measures for continuous variables if they are skewed, as skewed variables can
#     greatly affect ordinary correlation coefficeients.
#   - Pairwise deletion of missing values is also advisable for thie procedure:  casewise deletion can result in a small biased sample.
#   - When variables are not monotonically related to each other, Pearson or Spearman squared correlations can miss important associations and thus
#     are not always good similarity measures.
#     General and robust similarity measure is Hoeffding's D:  D statistic will detect a wide variety of dependencies between two variables.
# ------------------------------------------------------------------------------

library(Hmisc)


# We use Hoeffding's D: D will detect nonmonotonic associations
vc <- varclus(~ ., data = Rogers, sim="hoeffding")

plot(vc)


