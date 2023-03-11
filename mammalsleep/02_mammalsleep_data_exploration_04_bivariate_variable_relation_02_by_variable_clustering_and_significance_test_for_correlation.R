setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


dim(mammalsleep)


car::some(mammalsleep)



# ----------
# Calculate dream / sleep
mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ------------------------------------------------------------------------------
# data exploration:  Correlation plot and variable clustering
# ------------------------------------------------------------------------------

library(corrplot)


corrplot(cor(na.omit(mammalsleep)), method = "color", cl.length = 20, order = "AOE", addcoef.col = "grey")


corrplot(cor(na.omit(mammalsleep)), order = "hclust", hclust.method = "ward.D2", addrect = 3)



# ------------------------------------------------------------------------------
# data exploration:  Variable Clustering
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
vc <- varclus(~ ., data = mammalsleep, sim="hoeffding")

plot(vc)




# ------------------------------------------------------------------------------
# data exploration:
# Significante test wchi hproduces p-values and confidence intervals for each pair of input features
# ------------------------------------------------------------------------------

( cor_obj <- cor.mtest(mammalsleep, conf.level = 0.95, method = "pearson") )



# ----------
cor_obj_upper <- cor_obj$uppCI

cor_obj_lower <- cor_obj$lowCI

rownames(cor_obj_upper) <- colnames(mammalsleep)
colnames(cor_obj_upper) <- colnames(mammalsleep)
rownames(cor_obj_lower) <- colnames(mammalsleep)
colnames(cor_obj_lower) <- colnames(mammalsleep)



# ---------
crit <- 0.6

abs(cor_obj_upper) >= crit

abs(cor_obj_lower) >= crit
