setwd("//media//kswada//MyFiles//R//ucb_admissions")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UCBAdmissions
# ------------------------------------------------------------------------------

data("UCBAdmissions", package = "datasets")


data <- UCBAdmissions


data


dimnames(data)


dim(data)




# ------------------------------------------------------------------------------
# Quantify the strength of association between admission and gender
#   - We would treat the numbers of male and female applicants as fixed and
#     consider the cell frequencies to represent two independent binomial samples for a binary response
#     --> H0: homofeneity of the reponse proportions across the levels of the explanatory variable
#
# Odds Ratio / Log Odds Ratio  (Asymptotic test)
# ------------------------------------------------------------------------------

dat_tab <- margin.table(data, 1:2)

dat_tab




# ----------
# inference for the odds ratio is more conveniently carried out in terms of the log odds ratio,
# whose sampling distribution is more closely normal
# (sampling distribution of odds ration is asymptotically normal as n --> inf, but may be highly skewed in small to moderate samples)

( LOR <- loddsratio(dat_tab) )

( OR <- loddsratio(dat_tab, log=FALSE))




# ----------
# ASE (asymptotic standard error) and carries out the significance test (for the log odds)

summary(LOR)

summary(OR)


# --> clearly the hypothesis of independence has to be rejected, suggesting the presence of gender bias.




# ------------------------------------------------------------------------------
# Confidence intervals for (log) odds ratios
# ------------------------------------------------------------------------------

confint(OR)

confint(LOR)





# ------------------------------------------------------------------------------
# Quantify the strength of association between admission and gender
#
# Odds Ratio  (Fisher's exact test)
#  - In general, exact tests are to be prefered over asymptotic tests
# ------------------------------------------------------------------------------

fisher.test(dat_tab)


# -->
# Results are very similar of asymptotic test





# ------------------------------------------------------------------------------
# (Log) odds ratio varies with a stratifying factor (Department)
# ------------------------------------------------------------------------------

# The log odds ratio varies with a stratifying factor
# together with confidence interval error bars

lor <- loddsratio(data)

plot(lor, bars = FALSE, baseline = FALSE, whiskers = 0.2)


