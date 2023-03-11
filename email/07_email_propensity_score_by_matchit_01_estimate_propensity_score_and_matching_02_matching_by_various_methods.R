setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




library(MatchIt)


# ----------

formula1 <- treatment ~ recency + history + channel



# select data
data <- male_df

data <- biased_data





# ------------------------------------------------------------------------------
# Exact Matching
#   - The simplest version of matching is exact. This technique matches each treated unit to all possible control units with exactly
#     the same values on all the covariates, forming subclasses such that within each subclass all units (treatment and control) 
#     have the same covariate values. 
# ------------------------------------------------------------------------------

m.out_exact <- matchit(formula1, data = data, method = "exact")




# ------------------------------------------------------------------------------
# Subclassification
#   - When there are many covariates (or some covariates can take a large number of the values), finding sufficient exact matches will often be impossible. 
#     The goal of subclassification is toform subclasses, such that in each the distribution (rather than the exact values)
#     of covariates for the treated and control groups are as similar as possible.
#   - Various subclassification schemes exist, including the one based on a scalar distance measure such as the propensity score
#     estimated using the distance option
# ------------------------------------------------------------------------------


m.out_sub <- matchit(formula1, data = data, method = "subclass")



# -->
# As a default, this forms 6 subclasses, based on a distance measure (the propensity score) estimated using logistic regression.
# By default, each subclass will have approximately the same number of treated units.



# ------------------------------------------------------------------------------
# Nearest Neighbour Matching
#   - Subclassification may also be used in conjunction with nearest neighbor matching, by leaving the default ofmethod = "nearest"
#     but adding the option subclass. 
#   - Nearest neighbor matching selects the r (default = 1) best control matches for each individual in the treatment group
#     (excluding those discarded using the discard option). 
#   - Matching is done using a distance measure specified by the distance option (default = logit, i.e., a logistic regression model is used to estimate the propensity score, 
#     defined as the probability of receiving treatment, conditional on the covariates).
#     Matches are chosen for each treatedunit one at a time, with the order specified by the m.order command (default = largest to smallest).
#     At each matching step we choose the control unit that is not yet matched but is closest to the treated unit on the distance measure.
#   - The default nearest neighbor matching method in MatchIt is 窶徃reedy窶? matching, where the closest control match for each treated unit is chosen
#     one at a time, without trying to minimize a global distance measure. 
# ------------------------------------------------------------------------------

m.out_nn <- matchit(formula1, data = data, method = "nearest", replace = FALSE)




# ------------------------------------------------------------------------------
# Optimal Matching
#   - In contrast, optimal matching finds the matched samples with the smallest average absolute distance across all the matched pairs.
#     Gu and Rosenbaum (1993) find that greedy and optimal matching approaches generally choose the same sets of controls for the overall matched samples,
#     but optimal matching does a better job of minimizing the distance within each pair. In addition, optimal matching can be helpful
#     when there are not many appropriate control matches for the treated units. 
# ------------------------------------------------------------------------------

# We conduct 2:1 optimal ratio matching based on the propensity score from the logistic regression
m.out_opt <- matchit(formula1, data = data, method = "optimal", ratio = 2)



# ------------------------------------------------------------------------------
# Full matching
#   - Full matching is a particular type of subclassification that forms the subclasses in an optimal way (Rosenbaum 2002; Hansen 2004). 
#     A fully matched sample is composed of matched sets, where each matched set contains one treated unit and one or more controls
#     (or one controlunit and one or more treated units).  As with subclassification, the only units not placed intoa subclass will be those discarded
#     (if adiscardoption is specified) because they are outsidethe range of common support. 
#     Full matching is optimal in terms of minimizing a weighted average of the estimated distance measure between each treated subject
#     and each control subject within each subclass.
# ------------------------------------------------------------------------------

m.out_full <- matchit(formula1, data = data, method = "full")



# ------------------------------------------------------------------------------
# Genetic matching
#   - Genetic matching automates the process of finding a good matching solution (Diamond andSekhon  2010).
#     The idea is to use a genetic search algorithm to find a set of weights for each  covariate such that the a version of optimal balance is achieved 
#     after matching. As currently implemented, matching is done with replacement using the matching method of Abadie and Imbens (2006) and balance is
#     determined by two univariate tests, paired t-tests for dichotomous variables and a Kolmogorov-Smirnov test for multinomial and continuous variables,
#     but these options can be changed.
# ------------------------------------------------------------------------------

# data <- data[1:1000,]

# IT TAKES TIME !!!
m.out_gen <- matchit(formula1, data = data, method = "genetic")



# ------------------------------------------------------------------------------
# Checking balance
#   - The summary() command gives measures of the balance between the treated and control groups in the full (original) data set,
#     and then in the matched data set.
#     If the matching worked well, the measures of balance should be smaller in the matched data set (smaller values of the measures indicate better balance).
#   - The summary() output for subclassification is the same as that for other types of matching, except that the balance statistics are shown separately 
#     for each subclass, and the overall balance in the matched samples is calculated by aggregating across the subclasses, where each subclass is weighted
#     by the number of units in the subclass. 
#     For exact matching, the covariate values within each subclass are guaranteed to be the same, and so the measures ofbalance are not output for exact matching;
#     only the sample sizes in each subclass are shown.
#
#   - Balance statistics:
#       - The statistics the summary() command provides include means, the original control group standard deviation (where applicable),
#         mean differences, stan-dardized mean differences, and (median, mean and maximum) quantile-quantile (Q-Q) plot differences.
#       - In addition, the summary() command will report (a) the matched call,(b) how many units were matched, unmatched, or discarded due to the discard option, 
#         and (c) the percent improvement in balance for each of the balance measures, defined as 100((|a|竏竹b|)/|a|),
#         where a is the balance before and b is the balance after matching.
#       - For each set of units (original and matched data sets, with weights used as appropriate in the matched data sets), the following statistics are provided:
#           1. Means Treated and Means Control show the weighted means in the treated and control groups
#           2. SD Control is the standard deviation calculated in the control group (where applicable)
#           3. Mean Diff. is the difference in means between the groups
#           4. The final three columns of the summary output give summary statistics of a Q-Qplot.
#              Those columns give the median, mean, and maximum distance between the two empirical quantile functions (treated and control groups).
#              Values greater than 0 indicate deviations betweenthe groups in some part of the empirical distributions. 
#              The plots of the two empiri-cal quantile functions themselves, described below, can provide further insight into
#              which part of the covariate distribution has differences between the two groups.
#
#   - Additional options:
#       - interactions = TRUE option with summary() shows the balance of all squares and interactions of the covariates used in the matching procedure.
#         Large differences in higher order interactions usually are a good indication that the propensity score model (the distance measure) needs to be
#         respecified.
#       - the addlvariables option with summary() will provide balance measures on additional variables not included in the original matching procedure.
#         If a variable (or interaction of variables) not included in the original propensity score model has large imbalances in the matched groups,
#         including that variable in the next model specification may improve the resulting balance on thatvariable.
#         Because the outcome variable is not used in the matching procedure, a varietyof matching methods can be tried, and the one that leads to the best
#         resulting balancechosen.
#       - the standardize = TRUE option will print out standardized versions of the balance measures, where the mean difference is standardized (divided) by the SD in the origianl treated group
# ------------------------------------------------------------------------------

summary(m.out_exact)

summary(m.out_sub)

summary(m.out_nn)

summary(m.out_opt)

summary(m.out_full)

summary(m.out_gen)



# select models
mod_obj <- m.out_exact

mod_obj <- m.out_sub

mod_obj <- m.out_nn

mod_obj <- m.out_opt

mod_obj <- m.out_full

mod_obj <- m.out_gen



# -----------
mod_obj$nn



# -----------
# If the empirical distributions are the same in the treated and control groups,
# the points in the Q-Qplots would all lie on the 45 degree line
# Deviations from the 45 degree line indicate differences in the empirical distribution. 

plot(mod_obj)



# ----------
# jitter plot shows the overall distribution of propensity scores in the treated and control groups.
# the size of each point is proportional to the weight given to that unit.

# plot(mod_obj, type = "jitter")



# ----------
# histogram of propensity score

plot(mod_obj, type = "hist", col = gray(0.6))





# ------------------------------------------------------------------------------
# Assess covariate balance
# ------------------------------------------------------------------------------

library(cobalt)



# select model
mod_obj <- m.out_exact
mod_obj <- m.out_sub
mod_obj <- m.out_nn
mod_obj <- m.out_opt
mod_obj <- m.out_full
mod_obj <- m.out_gen


love.plot(mod_obj, stats = "mean.diffs", threshold = 0.1, stars = "std", size = 5, col = c(gray(0.7), "blue"))

bal.tab(mod_obj)



# -->
# Note that optimal matching and genetic matching achieves better covariance balance
# genetic matching is best  (all for nswdw_data, cps1_nsw_data, cps3_nsw_data)



# ------------------------------------------------------------------------------
# Create data after matching
# ------------------------------------------------------------------------------


matched_data_exact <- match.data(m.out_exact)
matched_data_sub <- match.data(m.out_sub)
matched_data_nn <- match.data(m.out_nn)
matched_data_opt <- match.data(m.out_opt)
matched_data_full <- match.data(m.out_full)
matched_data_gen <- match.data(m.out_gen)


matched_data_list <- list(matched_data_exact, matched_data_sub, matched_data_nn, matched_data_opt, matched_data_full, matched_data_gen)




# ------------------------------------------------------------------------------
# Estimate Average Treatment Effect on Treated (ATT)
# ------------------------------------------------------------------------------


library(broom)



# After matchit, ATT is estimated
# ATT: average treatment effect on the treated

for(i in 1:length(matched_data_list)){
  result <- matched_data_list[[i]] %>% lm(re78 ~ treat, data = .) %>% tidy()
  print(result)
}




summary(lm(data = nswdw_data, re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married))




# -->
# BUT genetic matching's ATT is only 1386 ...
# NN matching: 1654, close to RCT 1676



