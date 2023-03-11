setwd("//media//kswada//MyFiles//R//breastfeeding")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Rosenbaum's sensitivity analysis
#   - Sensitivity analysis consists of examining what magnitude of hidden bias would change inferences about a treatment effect.
#     Rosenbaum (2002) proposed a nonparametric sensitivity analysis method for continuous and ordinal outcomes based on the Wilcoxon signed ranks test.
#
#   - Rosenbaum's sensitivity analysis is based on the principle that if two cases have the same values on observed covariates but different probabilities
#     of treatment assignment, the odds ratio of these cases receiving the treatment is:
#     = { pi(j) / (1 - pi(j)) } / { pi(k) / (1 - pi(k)) } = { pi(j) / (1 - pi(k)) } / { pi(k) / (1 - pi(j)) }
#     If there is an unobserved confounder, the odds ratio will be larger than 1 and smaller than a constant gamma that measures the degree of departure
#     from the absence of hidden bias.
#     Therefore, the value of gamma can be manipulated to evaluate how large it has to be for inferences about the significance of the treatment effect to change.
#     If gamma has to attain very high values for inferences to change, then it is possible to conclude that the treatment effect is insensitive to hidden bias.
#     Rosenbaum's sensitivity analysis consists of computing p values of the lower and upper bounds of the Wilcoxon signed rank statistic for the
#     outcome difference between treated and untreated groups, under null hypotheses with increasing values of gamma.
#
#   - The rbounds package was designed to work together with the Matching package to implement Rosenbaum's sensitivity analysis method.
#     It can handle matched outcomes obtained with packages other than Matching, but it can currently handle only one-to-one and fixed ratio matching.
# ------------------------------------------------------------------------------
library(rbounds)


# Rosenbaum Sensitivity Test for Wilcoxon Signed Rank P-Value with genetic matching
# varying the sensitivity parameter gamma from 1 to a maximum givben by Gamma = 3, in increments of 0.1 specified by the GammaInc argument,
psens(geneticMatching, Gamma = 3, GammaInc = .1)


# -->
# The results show that although the p value assuming no hidden bias is not statistically significant, a value equal to 1.2 or larger 
# could lead to a significant p value, and therefore the conclusion that there is no effect of availability of company-provided or company-subsidized
# child care on length of breastfeeding is vulnerable to hidden bias.
# If the lower bound of the p value did not overlap the significance level even with gamma as high as 3, 
# then there would be evidence of the results not being sensitive to hidden bias.



#==================================================
#save all results
#save(list=ls(), file="analysis_results_breastfeeding_example.Rdata",compress=T)
