setwd("//media//kswada//MyFiles//R//punishment")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Punishment
# ------------------------------------------------------------------------------

data("Punishment", package = "vcd")


data <- Punishment


data


str(Punishment)



# ----------
# convert data.frame to table

tab <- xtabs(Freq ~ memory + attitude + age + education, data = data)


dimnames(tab) <- list(
  Memory = c("yes", "no"),
  Attitude = c("no", "moderate"),
  Age = c("15-24", "25-39", "40+"),
  Education = c("Elementary", "Secondary", "High")
)



# Memory * Attitude * Age * Education
tab




# ------------------------------------------------------------------------------
# Examine the odds ratios among attitude variable and any one predictor,
# treating the other variables as strata.
# ------------------------------------------------------------------------------
# We calculate log odds ratios for the association of attitude and memory, stratified by age and education
# loddsratio() in vcd package calculates log odds ratios for the categories of the 1st two dimensions of an n-way table,
# together with their asymptotic covariance matrix.

( pun_lor <- loddsratio(Freq ~ memory + attitude | age + education, data = data) )


confint(pun_lor)


summary(pun_lor)




# ----------
# the plot method for loddsratio objects convenierntly plots the log odds ratio (LOR) against
# the strata variables, age or education,
# by default also adds error bars.

plot(pun_lor)




# -->
# Differences among the age and education groups are now clear.
# For respondents less thatn age 40, increasing education increases the association (log odds ratio)
# between attitude and memory: those who remembered corporal punishment as a child are more likely to approve of it as their education increase.
# This result is reversed for those over 40, wher all log odds ratios are negative: 
# memory of corporal punishment makes it less likely to approve,
# and this effect becomes stronger with increased education.




# ------------------------------------------------------------------------------
# Rough analysis of the effects of the stratifying variables using ANOVA
# ------------------------------------------------------------------------------
# Because log odds ratios have an approximately normal distribution under the null hypothesis that all log theta(ij) = 0,
# you can treat these values as data, and carry out a rough analysis of the effects of the stratifying variables using ANOVA,
# with weights inversely proportional to the estimated sampling variances.

# But note that this ignores the covariances among the log odds ratios, which are not independent.
# A proper analysis uses generalized least squares with a weight matrix = 1 / S, where S = V[log(theta)] is the covariance matrix

( pun_lor_df <- as.data.frame(pun_lor) )

( pun_lor_df <- transform(pun_lor_df, 
                        age = as.numeric(age), 
                        education = as.numeric(education)) )


pun_mod <- lm(LOR ~ age * education, data = pun_lor_df, weights = 1 / ASE^2)

summary(pun_mod)


anova(pun_mod)


# -->
# This confirms the interaction of age and education on the association between attitude and memory



