# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "lmtest", "ggplot2", "directlabels", "effects", "car", "nnet")
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
# Set reference level
# ------------------------------------------------------------------------------
levels(data$partic)


# choose not working as baseline category
data$partic <- relevel(data$partic, ref = "not.work")
levels(data$partic)



# ------------------------------------------------------------------------------
# Generalized logit model
#  - This is used to construct submodels comparing any pair of categories
#  - We have assumed that the effects of husband's income and presence of young children are additive on the log odds scale
# ------------------------------------------------------------------------------
# specifying Hess = TRUE saves the Hessian and facilitates calculation of standard errors and hypothesis tests.
wlf.multinom <- nnet::multinom(partic ~ hincome + children, data = data, Hess = TRUE)


# The summary() method for "multinom" objects does not calculate test statistics for the estimatedd coeffs by default
# The option Wald = TRUE produces Wald z-test statistics, calculated as z = beta / SE(beta)
summary(wlf.multinom)
( stats <- summary(wlf.multinom, Wald = TRUE) )


# --> The first line in each table pertains to the logit comparing full time work with the not working reference level.
# The second line compares part time work against not working.



# ----------
# p-values for significance tests (based on standard normal asymptotic approximation)
# Both husband's income and presence of children have highly significant effects on the comparison of working full time as opposed to not working,
# while neither of these predictors are significant for the comparison of working part time vs. not working.
z <- stats$Wald.ratios
( p <- 2 * (1 - pnorm(abs(z))) )
zapsmall(p)



# ------------------------------------------------------------------------------
# Generalized logit model
#  - Allowing an interaction to compare with additive model above in order to test the significance of additive assumption
# ------------------------------------------------------------------------------
# The test for the interaction term, hincom:children, is not significant
wlf.multinom2 <- multinom(partic ~ hincome * children, data = data, Hess = TRUE)
car::Anova(wlf.multinom2)



# ------------------------------------------------------------------------------
# Generalized logit model
#  - Fitted probabilities
# ------------------------------------------------------------------------------
predictors <- expand.grid(hincome = 1 : 50, children = c("absent", "present"))
fit <- data.frame(predictors, predict(wlf.multinom, predictors, type = "probs"))


fit2 <- reshape2::melt(fit,
             measure.vars = c("not.work", "fulltime", "parttime"),
             variable.name = "Participation",
             value.name = "Probability")
levels(fit2$Participation) <- c("not working", "full-time", "part-time")


gg <- ggplot(fit2, aes(x = hincome, y = Probability, colour = Participation)) + facet_grid(~ children) + 
  geom_line(size = 2) + theme_bw() + scale_x_continuous(limits = c(-3, 50)) + scale_y_continuous(limits = c(0, 0.9))   
gg


# gg <- ggplot(fit2, aes(x = hincome, y = Probability, colour = Participation)) + facet_grid(~ children, labeller = function(x, y) sprintf("%s = %s", x, y)) + 
#  geom_line(size = 2) + theme_bw() + scale_x_continuous(limits = c(-3, 50)) + scale_y_continuous(limits = c(0, 0.9))   
# direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))


# --> roughly similar to those obtainesd from the nested dichotomy models.
# However, the predicted probabilities of of not working under the generalized logit model rize more steeply with hudband's income for women with no children
# and level off sooner for women with young children.



# ------------------------------------------------------------------------------
# Effect plot for the probabilities of not working and working part time and full time
# ------------------------------------------------------------------------------
# Arrange levels in their natural order
levels(data$partic)
data$partic <- ordered(data$partic, levels=c("not.work", "parttime", "fulltime"))
levels(data$partic)


# new fitted model for arranged levels
wlf.multinom <- update(wlf.multinom, . ~ .)



# ----------
# Stacked effect model
# It is analogous to the full-model plot shown above
plot(effects::Effect(c("hincome", "children"), wlf.multinom), style = "stacked", key.args = list(x = .05, y = .9))


