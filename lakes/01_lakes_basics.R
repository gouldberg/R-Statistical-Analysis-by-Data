setwd("//media//kswada//MyFiles//R//lakes")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Lakes
#   - Dataset taken from Lakes and Hoyt (2009). The authors used the RCS (response to challenge scale) in order to assess children's self-regulation in response
#     to a physically challenging situation. The scale consists of three domains: cognitive, affective/motivational, and physical.
# ------------------------------------------------------------------------------

data("Lakes", package = "MPsychoR")


str(Lakes)


car::some(Lakes)



# ----------
# Here we focus on the physical domain only
# Each of the 194 children in the sample is rated by five raters on three items on his/her self-regulatory ability.
# The ratings are on a scale from 1 to 7.

phydat <- subset(Lakes, subtest == "physical")

phydat$item <- droplevels(phydat$item)

head(phydat)

car::some(phydat)


