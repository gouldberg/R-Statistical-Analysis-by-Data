# setwd("//media//kswada//MyFiles//R//european_valuesstudy")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//european_valuesstudy")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  European Values Study
#   - A sample of the 1999 European Values Study (EVS) containing an assessment of materialism/postmaterialism in 3584 respondents from 32 countries.
#   - The data are part of a larger survey conducted in 1999 in 32 countries in Europe (see http://www.europeanvaluesstudy.eu/).
#     Vermunt (2003) obtained a sample from 10 percent of the available cases per country, yielding 3584 valid cases.
#     The item in the 1999 European Values Study questionnaire aiming at recording materialism/postmaterialism reads as follows:
#       -  There is a lot of talk these days about what the aims of this country should be for the next ten years.
#          On this card are listed some of the goals which different people would give top priority.
#          If you had to choose, which of the things on this card would you say is most important? And which would be the next most important?
#          A: Maintaining order in the nation
#          B: Giving people more say in important government decisions
#          C: Fighting rising prices
#          D: Protecting freedom of speech
#     The double-choice task implies a partial ranking of the alternatives and (assuming transitivity) an incomplete set of paired comparisons for each respondent.
#     The country group according to postmaterialism was derived by Vermunt (2003) using a latent class model,
#     and applied by Lee and Lee (2010) in a tree model.
#   - variable "paircomp":  Paired comparison of class paircomp.
#     Five pairwise choices among four important political goals derived from a double-choice task (see Details).
# ------------------------------------------------------------------------------

data("EuropeanValuesStudy", package = "psychotree")


str(EuropeanValuesStudy)



# -->
# Note that "paircomp" has "paircomp" class 



car::some(EuropeanValuesStudy)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
