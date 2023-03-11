setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)

str(Vietnam)


car::some(Vietnam)



# ----------
levels(Vietnam$response)


# choose "A" clearly as baseline category
Vietnam$response <- relevel(Vietnam$response, ref = "A")

levels(Vietnam$response)




# ------------------------------------------------------------------------------
# Generalized logit model by nnet::miltinom()
# ------------------------------------------------------------------------------

Vietnam$year2 <- Vietnam$year * (Vietnam$sex == "Male")


# no effect of year for females and the effect of year is linear for males
viet.multinom3 <- nnet::multinom(response ~ sex + year2, data = Vietnam, Hess = TRUE, weights = Freq)

# viet.multinom3 <- nnet::multinom(response ~ sex + (sex == "Male") : year, data = Vietnam, Hess = TRUE, weights = Freq)


# ----------
summary(viet.multinom3)

( stats3 <- summary(viet.multinom3, Wald = TRUE) )




# ----------
# p-values for significance tests (based on standard normal asymptotic approximation)
z3 <- stats3$Wald.ratios

( p3 <- 2 * (1 - pnorm(abs(z3))) )

zapsmall(p3)



# ----------
# Assess the model terms
car::Anova(viet.multinom3)



# ------------------------------------------------------------------------------
# Fitted probabilities
# ------------------------------------------------------------------------------

predictors_m <- expand.grid(sex = c("Male"),
                          year2 = c(1, 2, 3, 4, 5))


# Female have no year effect
predictors_f <- expand.grid(sex = c("Female"),
                            year2 = c(0))

predictors <- rbind(predictors_m, predictors_f)


( fit <- data.frame(predictors, predict(viet.multinom3, predictors, type = "probs")) )


fit2 <- reshape2::melt(fit,
                       measure.vars = c("A", "B", "C", "D"),
                       variable.name = "response",
                       value.name = "Probability")

levels(fit2$response) <- c("A", "B", "C", "D")

fit2



# ------------------------------------------------------------------------------
# Full-model plot by ggplot
# ------------------------------------------------------------------------------
library(ggplot2)


gg <- ggplot(fit2, aes(x = year2, y = Probability)) + geom_line(aes(colour = response), size = 1.8) + geom_point(colour = "black", size = 1.2) + theme_bw() + facet_grid(~ sex)

gg



# ------------------------------------------------------------------------------
# main effect plot is MISS LEADING ...
# ------------------------------------------------------------------------------


library(effects)


plot(allEffects(viet.multinom3))


