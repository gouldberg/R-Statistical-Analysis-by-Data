setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HarvardPsych
# ------------------------------------------------------------------------------

data("HarvardPsych", package = "MPsychoR")

str(HarvardPsych)

dim(HarvardPsych)


# researchers in rows, words in columns  (29 * 43)
head(HarvardPsych)



# ----------
rownames(HarvardPsych)

colnames(HarvardPsych)




# ------------------------------------------------------------------------------
# Multidimensional Scaling:  by research theme
# ------------------------------------------------------------------------------

library(smacof)


mat_dist <- psych::cor2dist(cor(HarvardPsych))


fit_mds <- mds(mat_dist, type = "interval")


fit_mds



# -->
# stress-1 value = 0.312: medium high.. 




summary(fit_mds)




# ----------
plot(fit_mds)



# -->
# Note that memory is almost center
# language is at edge



# ----------
plot(fit_mds, plot.type = "stressplot")




# ------------------------------------------------------------------------------
# Multidimensional Scaling:  by researchers
# ------------------------------------------------------------------------------

mat_dist2 <- psych::cor2dist(cor(t(HarvardPsych)))


fit_mds2 <- mds(mat_dist2, type = "interval")


fit_mds2

summary(fit_mds2)




# ----------
plot(fit_mds2)



# -->
# Note that Schacter is very close to Buckner



# ----------
HarvardPsych[c("Schacter", "Buckner"),]


cor(t(HarvardPsych[c("Schacter", "Buckner"),]))


# -->
# Note that both researchers have large values in "memory"




# ----------
# Center "Hooley"
HarvardPsych[c("Hooley"),]

