setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "cluster", "dendextend", "pvclust", "labdsv", "RColorBrewer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



# ----------
# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
latlong <- latlong[-8,]



# ------------------------------------------------------------------------------
# Comparing a typology with external data (ANOVA approach)
#
# 4 environmental variables are significantly different among species clusters ?
#  - Elevation, Slope, Oxygen, and Ammonium (after some transformations)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Normality test of residuals
# ------------------------------------------------------------------------------

shapiro.test(resid(aov(sqrt(env$ele) ~ as.factor(spech.ward.gk))))

shapiro.test(resid(aov(log(env$slo) ~ as.factor(spech.ward.gk))))

shapiro.test(resid(aov(env$oxy ~ as.factor(spech.ward.gk))))

shapiro.test(resid(aov(sqrt(env$amm) ~ as.factor(spech.ward.gk))))


# -->
# normally distributed



# ------------------------------------------------------------------------------
# Homogeneity of variances:  parametric Bartlett test
# ------------------------------------------------------------------------------

bartlett.test(sqrt(env$ele) ~ as.factor(spech.ward.gk))

bartlett.test(log(env$slo) ~ as.factor(spech.ward.gk))

bartlett.test(env$oxy ~ as.factor(spech.ward.gk))

bartlett.test(sqrt(env$amm) ~ as.factor(spech.ward.gk))


# -->
# variable sqrt(ele) has heterogeneous variances, not appropriate for parametric ANOVA



# ------------------------------------------------------------------------------
# Homogeneity of variances:  permutation and bootstrap Bartlett test
# Parametric Bartlett test is sensitive to departures from normality.
# For non-normal data, batrlett.perm.R computes parametric, permutation and bootstrap Bartlett test.
# ------------------------------------------------------------------------------

source("./functions/bartlett.perm.R")

bartlett.perm(sqrt(env$ele), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)

bartlett.perm(log(env$slo), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)

bartlett.perm(env$oxy, as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)

bartlett.perm(sqrt(env$amm), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)



# ------------------------------------------------------------------------------
# Environmental variables are significantly different among species clusters
# ------------------------------------------------------------------------------

summary(aov(log(env$slo) ~ as.factor(spech.ward.gk)))

summary(aov(env$oxy ~ as.factor(spech.ward.gk)))

summary(aov(sqrt(env$amm) ~ as.factor(spech.ward.gk)))

kruskal.test(env$ele ~ as.factor(spech.ward.gk))



# ------------------------------------------------------------------------------
# post-hoc tests and show the results
# ------------------------------------------------------------------------------
# Different letters denote significant differences among groups
# boxplert(): perform ANOVA and LSD tests for multiple comparisons
# boxplerk(): perform Kruskal-Wallis test and its corresponding post-hoc comparisons (both with Holm correction)

par(mfrow = c(2, 2))

boxplot(sqrt(env$ele) ~ spech.ward.gk, main = "Elevation", las = 1, ylab = "sqrt(alt)", col = (1:k) + 1, varwidth = TRUE)

boxplot(log(env$slo) ~ spech.ward.gk, main = "Slope", las = 1, ylab = "log(slo)", col = (1:k) + 1, varwidth = TRUE)

boxplot(env$oxy ~ spech.ward.gk, main = "Oxygen", las = 1, ylab = "oxy", col = (1:k) + 1, varwidth = TRUE)

boxplot(sqrt(env$amm) ~ spech.ward.gk, main = "Ammonium", las = 1, ylab = "sqrt(amm)", col = (1:k) + 1, varwidth = TRUE)



# ----------
source("./functions/boxplerk.R")

source("./functions/boxplert.R")

par(mfrow = c(2, 2))

boxplerk(env$ele, spech.ward.gk, xlab = "", ylab = "ele", main = "Elevation", bcol = (1:k) + 1, p.adj = "holm")

boxplert(log(env$slo), spech.ward.gk, xlab = "", ylab = "log(slo)", main = "Slope", bcol = (1:k) + 1, p.adj = "holm")

boxplert(env$oxy, spech.ward.gk, xlab = "", ylab = "oxy", main = "Oxygen", bcol = (1:k) + 1, p.adj = "holm")

boxplert(sqrt(env$amm), spech.ward.gk, xlab = "", ylab = "sqrt(amm)", main = "Ammonium", bcol = (1:k) + 1, p.adj = "holm")



