# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//consumer_segment")

packages <- c("dplyr", "Hmisc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Consumer Segment Data (created data)
# ------------------------------------------------------------------------------

seg.df <- read.csv("rintro-chapter5.csv", header = TRUE)


str(seg.df)


Hmisc::describe(seg.df)

summary(seg.df)

car::some(seg.df)


# remove the known segment assignments
seg.df2 <- seg.df[, -7]


# 300 * 6
dim(seg.df2)



# ---------------------------------------------------------------------------
# Latent Class Analysis: poLCA()
#   - poLCA() uses only catgorical variables.
# ---------------------------------------------------------------------------

# Convert numeric data to factors:  here we simply recode everything as binary
seg.df.cut <- seg.df2 %>% mutate(
  age = factor(ifelse(age < median(age), 1, 2)),
  income = factor(ifelse(income < median(income), 1, 2)),
  kids = factor(ifelse(kids < median(kids), 1, 2))
)


  
summary(seg.df.cut)



# ----------
# poLCA() can estimate complex models with covariates, but here we only wish to examine the effect of cluster membership alone.
# Thus, we model the dependent variables (all the observed columns) with respect to the model intercepts
# ~1: to specify a formula with intercepts only
seg.f <- with(seg.df.cut, cbind(age, gender, income, kids, ownHome, subscribe))

seg.f



# ----------
library(poLCA)

set.seed(02807)

seg.LCA3 <- poLCA(seg.f, data = seg.df.cut, nclass = 3)
seg.LCA4 <- poLCA(seg.f, data = seg.df.cut, nclass = 4)


seg.LCA3$bic
seg.LCA4$bic


# -->
# The 3-cluster model shows a lower BIC by 29 and thus a substantially stronger fit to the data.



# ---------------------------------------------------------------------------
# check the clusters variation
# ---------------------------------------------------------------------------
# segment group statistics
seg.summ <- function(data, groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}



seg.summ(seg.df2, seg.LCA3$predclass)

table(seg.LCA3$predclass)


seg.summ(seg.df2, seg.LCA4$predclass)

table(seg.LCA4$predclass)



# ----------
seg.df2$cluster <- seg.LCA3$predclass


library(lattice)

boxplot(seg.df.num$age ~ seg.k$cluster, ylab = "Age", xlab = "Cluster")
boxplot(seg.df.num$kids ~ seg.k$cluster, ylab = "Kids", xlab = "Cluster")
boxplot(seg.df.num$income ~ seg.k$cluster, ylab = "Income", xlab = "Cluster")

histogram(~ gender | cluster, data = seg.df2, layout = c(4,1), col = c("burlywood", "darkolivegreen"))
histogram(~ ownHome | cluster, data = seg.df2, layout = c(4,1), col = c("burlywood", "darkolivegreen"))



# ---------------------------------------------------------------------------
# Visualize the clusters
#   - clusplot() will perform dimensional reduction with principal components or multidimensional scaling as the data warrant,
#     and then plot the observations with cluster membership identified.
# ---------------------------------------------------------------------------

library(cluster)


# lines = 0: omit distance lines between groups
clusplot(seg.df2, seg.LCA3$predclass, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "LCA plot (k = 3")

clusplot(seg.df2, seg.LCA4$predclass, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "LCA plot (k = 4)")







