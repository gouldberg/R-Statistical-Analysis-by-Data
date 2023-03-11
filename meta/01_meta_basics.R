setwd("//media//kswada//MyFiles//R//meta")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  meta
#  - Silagy et al. (2004) report a meta-analysis of 27 clinical trials of nicotine replacement therapy for smoking cessation.
#    In each trial, the participant was randomized to a treatment or control group.
#    The treatment group were given a nicotine gum.
#    In the majority of studies, the control group received gum having the same appearance but without the active ingredients,
#    but in some trials they were given no gum.
#    The outcome, whether the participant had quit smoking or not, was observed after six months.
#  - Variables:
#       - studyname:  location of the study (note that studyname is the same ffor studies at the same place in different years)
#       - year:  the year of the study
#       - d:  the number of quitters (non-smokers) after six months
#       - n:  the total number of participants
#       - fac:  a factor with two levels indicating whether control (1) or treatment (2)
#       - study:  numeric from 1 to 27 identifying the study (that is, the combination of studyname and year)
#
#  - The first 27 cases record the variable values for the control groups, and the second 27 cases for the treatment groups.
# ------------------------------------------------------------------------------
data("meta", package = "gamlss.data")


str(meta)

car::some(meta)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

library(lattice)


# sample log odds of quitting
meta$logitd <- with(meta, log(d / (n - d)))


# plot sample log odds of quitting by fac (control(1) and treatment(2))
with(meta, xyplot(logitd ~ fac, groups = study, type = "a",
                  auto.key = list(space = "no", points = FALSE, lines = TRUE),
                  scales = list(cex = 1.5, tick.number = 5)), ylab = list(cex = 1.5), xlab = list(cex = 1.5))



