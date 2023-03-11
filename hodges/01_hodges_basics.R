setwd("//media//kswada//MyFiles//R//hodges")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hodges
#  - The data were analyzed in Hodges (1998), who used a conditional normal model for Y(ij) given gamma(j) (the random effect in the mean for state j)
#    and a normal distribution for gamma(j)
#  - Variables:
#       - state:  a factor with 45 levels
#       - plan:  a two-character code that identifies plans (within state) declared here as factor with 325 levels
#       - prind:  the total health insurance premium for an individual
#       - prfam:  the total premium for a family
#       - enind:  the total enrolment of federal employees as individuals
#       - enfam:  the total enrolment of federal employees as families
# ------------------------------------------------------------------------------

data("hodges", package = "gamlss.data")


str(hodges)

car::some(hodges)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,1))

with(hodges, plot(prind ~ state))


# -->
# In the discussion of Hodges (1998), Wakefield commented: "If it were believed that there were different within-state variances then one possibility would
# be to assume a hierarchy for these also", while in his reply Hodges also suggested
# treating the "within-state precisions or variances as draws from some distribution".