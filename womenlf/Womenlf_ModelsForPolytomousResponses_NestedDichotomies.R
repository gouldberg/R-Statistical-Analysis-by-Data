# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
packages <- c("dplyr", "vcd", "vcdExtra", "lmtest", "ggplot2", "directlabels", "effects", "car")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Womenlf
#  - The resulf of a 1977 Canadian survey, containing data for 263 married women of age 21-30 who indicated their working status (outside the home)
#    as not working, working part time, or working full time, together with their husband's income and a binary indicator of whether they had one or more young
#    children in their household.
# ------------------------------------------------------------------------------
data("Womenlf", package = "car")

dim(Womenlf)
str(Womenlf)


car::some(Womenlf)


data <- Womenlf



# ------------------------------------------------------------------------------
# Create dichotomies
# ------------------------------------------------------------------------------
# It makes sense to consider a first dichotomy (working) between women who are not working vs. those who are (full time or part time)
# A second dichotomy (fulltime) contrasts full time work vs. part time work, among those women who are working at least part time.
data <- within(data, {
  working <-  recode(partic, " 'not.work' = 'no'; else = 'yes' ")
  fulltime <- recode(partic, " 'fulltime' = 'yes'; 'parttime' = 'no'; 'not.work' = NA")})


some(data)



# ----------
# how the response partic relates to the recoded binary variables, working and fulltime.
# Note that the fulltime variable is recoded to NA for women who are not working. 
with(data, table(partic, working))
with(data, table(partic, fulltime, useNA = "ifany"))



# ------------------------------------------------------------------------------
# Logistic regression nested dichotomies model
#  - This method does not require an ordinal response or special software. Instead, it uses the familiar binary logistic model and fits m - 1 separate models
#    for each of a hierarchically nested set of comparisons among the response categories.
#    Fit two separate binary logistic regression models for the derived dichotomous variables.
# ------------------------------------------------------------------------------
# For the working dichotomy
mod.working <- glm(working ~ hincome + children, family = binomial, data = data)
summary(mod.working)



# ----------
# For the fulltime dichotomy
mod.fulltime <- glm(fulltime ~ hincome + children, family = binomial, data = data)
summary(mod.fulltime)



# ----------
# For both dichotomies, increasing income of the husband and the presence of young children decrease the log odds of a greater level of work.
# However, for those women who are working, the effects of husband's income and children are greater on the choice between full time and part time work
# than hey are for all womena on the choice between working and not working.
cbind(working = coef(mod.working), fulltime = coef(mod.fulltime))



# ------------------------------------------------------------------------------
# Likelihood ratio test for all predictors simultaneously
# Tests of individual predictors
# Calculate X^2 statistics
#
#  - The use of nested dichotomies implies that the models fit to the separate dichotomies are statistically independent.
#    Thus, we can additively combine X^2 statistics and degrees of freedom to give overall tests for the polytomous response
# ------------------------------------------------------------------------------
# Calculate the likelihood ratio test of the hypothesis H0: beta = 0 for all predictors simultaneously
LRtest <- function(model) c(LRchisq = model$null.deviance - model$deviance, df = model$df.null - model$df.residual)

tab <- rbind(working = LRtest(mod.working), fulltime = LRtest(mod.fulltime))
tab <- rbind(tab, All = colSums(tab))
tab <- cbind(tab, pvalue = 1- pchisq(tab[,1], tab[,2]))
tab



# ----------
# Tests of individual predictors H0: beta(i) = 0, for the polytomy by adding the separate X^2 from Anova()
# The test for husband's income gives X^2 = 4.826 + 8.981 = 13.807 with 2df. 
car::Anova(mod.working)
car::Anova(mod.fulltime)



# ------------------------------------------------------------------------------
# Predicted probabilities and logit from the models for nested dichotomies fit
# ------------------------------------------------------------------------------
# Predicted probabilities and logit
predictors <- expand.grid(hincome = 1 : 50, children =c('absent', 'present'))
fit <- data.frame(predictors,
                  p.working = predict(mod.working, predictors, type = "response"),
                  p.fulltime = predict(mod.fulltime, predictors, type = "response"),
                  l.working = predict(mod.working, predictors, type = "link"),
                  l.fulltime = predict(mod.fulltime, predictors, type = "link")
)
print(some(fit, 5), digits = 3)



# ----------
# Calculate the unconditional probabilities
( fit <- within(fit, {
  `full-time` <- p.working * p.fulltime
  `part-time` <- p.working * (1 - p.fulltime)
  `not working` <- 1 - p.working
}) )


# Convert from wide to long format for ggplot2
( fit2 <- reshape2::melt(fit,
             measure.vars = c("full-time", "part-time", "not working"),
             variable.name = "Participation",
             value.name = "Probability") )


# ----------
# Fitted probabilities from the models for nested dichotomies fit
gg <- ggplot(fit2, aes(x = hincome, y = Probability, colour= Participation)) + facet_grid(~ children) + 
  geom_line(size = 2) + theme_bw() + scale_x_continuous(limits = c(-3, 55)) + scale_y_continuous(limits = c(0, 1))
gg


# gg <- ggplot(fit2, aes(x = hincome, y = Probability, colour= Participation)) + facet_grid(~ children, labeller = function(x, y) sprintf("%s = %s", x, y)) + 
#  geom_line(size = 2) + theme_bw() + scale_x_continuous(limits = c(-3, 55)) + scale_y_continuous(limits = c(0, 1))
# direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))


# --> We can see that the decision not to work outside the home increases strongly with husband's income,
# and is higher when there are children present.
# As well, among working women, the decision to work full time as opposed to part time decreases strongly with husband's income,
# and is less likely with young children.


# ----------
# Fitted logit (log odds) for the two dichotomies
fit3 <- reshape2::melt(fit,
             measure.vars = c("l.working", "l.fulltime"),
             variable.name = "Participation",
             value.name = "LogOdds")

levels(fit3$Participation) <- c("working", "full-time")

gg <- ggplot(fit3, aes(x = hincome, y = LogOdds, colour = Participation)) + facet_grid(~ children) + 
  geom_line(size = 2) + theme_bw() + scale_x_continuous(limits = c(-3, 50)) + scale_y_continuous(limits = c(-5, 4))
gg


# gg <- ggplot(fit3, aes(x = hincome, y = LogOdds, colour = Participation)) + facet_grid(~ children, labeller = function(x, y) sprintf("%s = %s", x, y)) + 
#  geom_line(size = 2) + theme_bw() + scale_x_continuous(limits = c(-3, 50)) + scale_y_continuous(limits = c(-5, 4))
# direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))


# --> It shows how the choice of full time work as opposed to part time depends more strongly on husband's income
# among women who are working than does the choice of working at all among all women.

# --> The proportional odds assumption would not be reasonable for data; that would require equal slopes for the two lines within each panel.


