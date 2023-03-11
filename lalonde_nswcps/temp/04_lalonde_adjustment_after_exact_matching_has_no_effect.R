setwd("//media//kswada//MyFiles//R//lalonde")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lalonde
# ------------------------------------------------------------------------------

data("lalonde")

str(lalonde)



# ------------------------------------------------------------------------------
# Adjustment after exact matching has no effect
#   - Regression adjustment after exact one-to-one exact matching gives the identical answer as a simple, unadjusted difference in means.
#     General exact matching, as implemented in MatchIt, allows one-to-many matches,  so to see the same result we must weight when adjusting.
#     In other words: weighted regression adjustment after general exact matching gives the identical answer as a simple, unadjusted weighted difference in means.
# ------------------------------------------------------------------------------

m.out <- matchit(treat ~ educ + black + hispan, data = lalonde, method = "exact")

m.data <- match.data(m.out)

weighted.mean(m.data$re78[m.data$treat == 1], m.data$weights[m.data$treat == 1]) - weighted.mean(m.data$re78[m.data$treat == 0], m.data$weights[m.data$treat == 0])

zelig(re78 ~ treat, data = m.data, model = "ls", weights = "weights")

zelig(re78 ~ treat + black + hispan + educ, data = m.data, model = "ls", weights = "weights")

