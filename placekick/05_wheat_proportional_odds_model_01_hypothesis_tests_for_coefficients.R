setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)



# ------------------------------------------------------------------------------
# Proportional odds model
# ------------------------------------------------------------------------------

# Order properly the type factor
levels(wheat$type)

wheat$type.order <- factor(wheat$type, levels = c("Scab", "Sprout", "Healthy"))

levels(wheat$type.order)



# ----------
library(MASS)

# The method = "logistic" argument value instructs to use the logit transformation on the cumulative probabilities
mod.fit.ord <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat, method = "logistic")
# mod.fit.ord2 <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat)

summary(mod.fit.ord)
# summary(mod.fit.ord2)



# ------------------------------------------------------------------------------
# Anova() to LRTs
# ------------------------------------------------------------------------------

library(car)

Anova(mod.fit.ord)


# -->
# Because of the large test statistic values for density and weight, there is sufficient evidence that these are important.
# There is marginal evidence that hardness is important too.



# ------------------------------------------------------------------------------
# Other ways to do LRTs
# ------------------------------------------------------------------------------

mod.fit.ord.class <- polr(formula = type.order ~ density + hardness + size + weight + moisture, data = wheat, method = "logistic")
anova(mod.fit.ord.class, mod.fit.ord)

test.stat <- mod.fit.ord.class$deviance - mod.fit.ord$deviance
df <- mod.fit.ord.class$df.residual - mod.fit.ord$df.residual
1 - pchisq(q = test.stat, df = df)  # LRT p-value



# ------------------------------------------------------------------------------
# Wald p-value
# ------------------------------------------------------------------------------
sum.fit <- summary(mod.fit.ord)

2 * (1 - pnorm(q = sum.fit$coefficients[1,3]))  # Wald p-value

