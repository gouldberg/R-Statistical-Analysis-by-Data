setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------
data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
# We shall take as our response he math test score result from the final year and try to model this as a function of gender,
# social class and the Raven's test score from the first year which mith be taken as a measure of ability when entering the school.

jspr <- jsp[jsp$year == 2,]



# ----------
# We express the multivariate response (math and english) for each individual by introducing an additional level of nesting at the individdual level
# We set up the data in a format with one test score per line with an indicator subject identifying which type of test was taken.
# We scale the English and math test scores by their maximum possible values, 40 and 100, respectively, to aid comparison
mjspr <- data.frame(rbind(jspr[,1:6], jspr[,1:6]), subject = factor(rep(c("english", "math"), c(953, 953))), score =  c(jspr$english/100, jspr$math/40))

head(mjspr)



# ------------------------------------------------------------------------------
# Examine the relationship between subject, gender and scores
# ------------------------------------------------------------------------------

ggplot(mjspr, aes(x = raven, y = score)) + geom_jitter(alpha = 0.25) + facet_grid(gender ~ subject)




# ------------------------------------------------------------------------------
# Multiple response multilevel model
# ------------------------------------------------------------------------------

mjspr$craven <- mjspr$raven - mean(mjspr$raven)


# Random Effects:  school, class and students are random effects with other terms being fixed effects
mmod_mul <- lmer(score ~ subject * gender + craven * subject + social + (1 | school) + (1 | school : class) + (1 | school : class : id), mjspr)



# ----------
summary(mmod_mul)



# -->
# Fixed Effects:
#   - The math subject scores were about 37% higher than the English scores.
#     This may just reflect the grading scale amd difficulty of the test amd so perhaps nothing in particular should be concluded from this except
#     that it is necessary to have this term in the model to control for this difference.

#  - Since gender has a significant interaction with subject, we must interpret these terms together.
#    On the English test, girls score 6.3% higher than boys. On the math test, the difference is 6.3 - 5.9 = 0.4% which is negligible.

#  - Scores are strongly related to the entering Raven score although the relation is slightly less strong for math than English
#    (slope is 0.017 for English but 0.017 - 0.004 for math)

#  - Declining performance as we move down the social class scale



# ------------------------------------------------------------------------------
# Test the fixed effects by Knward-Roger F-test degrees of freedom adjustment
# ------------------------------------------------------------------------------

library(pbkrtest)


# Test for a subject by gender interaction.
# subject and craven interaction
mmod <- lmer(score ~ subject * gender + craven * subject + social + (1 | school) + (1 | school : class) + (1 | school : class : id), data = mjspr, REML = FALSE)


# without interaction of subject and craven
mmodr <- lmer(score ~ subject * gender + craven + subject + social + (1 | school) + (1 | school : class) + (1 | school : class : id), data = mjspr, REML = FALSE)


KRmodcomp(mmod, mmodr)


# -->
# We can see that this effect is strongly statistically significant.



# ------------------------------------------------------------------------------
# Diagnostics:  Residuals vs. Fitted
# ------------------------------------------------------------------------------

diagd <- fortify(mmod)

ggplot(diagd, aes(x = .fitted, y = .resid)) + geom_point(alpha = 0.3) + geom_hline(yintercept = 0) + facet_grid(~ subject) + xlab("Fitted") + ylab("Residuals")



# -->
# We can see that the standard deviation of the residuals error in the math scores is smaller than that seen in the English scores.
# Perhaps this can be ascribed to the greater ease of consistent grading of math assignments or
# perhaps just greater variation is to be expected in English performance.

# Truncation effect of the maximum score is particularly visible for the math scores.


# ----------
# The correlation between the English and math scores after adjusting for the other effects is also of interest.
summary(mmod_mul)

0.0102521 / (0.0102521 + 0.0135916)


# -->
# Given a moderate positive correlation between the scores.
