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
# multinominal logit model
# ------------------------------------------------------------------------------

levels(wheat$type)


library(nnet)

methods(class = multinom)

mod.fit <- multinom(formula = type ~ class + density + hardness + size + weight + moisture, data = wheat)


summary(mod.fit)



# ------------------------------------------------------------------------------
# Confidence intervals for pi_j
#   - Computing confidence intervals for pi_j is not easy. The predict() function does not provide the estimated variances needed to compute
#     a probability distribution themselves.
#     A joint confidence region would be needed in J-1 dimensions to properly obtain an interval for these probabilities.
#   - Because a joint confidence region can be difficult to calculate and interpret, an alternative is one-at-a-time confidence intervals for each pi_j
#     and use these as an approximation. (Wald confidence intervals through the help of the deltaMethod())
#
#   - The deltaMethod() function of the car package provides a convenient way to apply the delta method to find variances of mathematical functions.
# ------------------------------------------------------------------------------

# Take first record
x1 <- 0
x2 <- wheat[1,2]
x3 <- wheat[1,3]
x4 <- wheat[1,4]
x5 <- wheat[1,5]
x6 <- wheat[1,6]


# Parts of character string
scab <- "exp(b20 + b21 * x1 + b22 * x2 + b23 * x3 + b24 * x4 + b25 * x5 + b26 * x6)"
sprout <- "exp(b30 + b31 * x1 + b32 * x2 + b33 * x3 + b34 * x4 + b35 * x5 + b36 * x6)"



# ----------
# pi.hat_Healthy

# Direct way
# g.healthy <- "1/(1 + exp(b20 + b21*x1 + b22*x2 + b23*x3 + b24*x4 + b25*x5 + b26*x6) + exp(b30 + b31*x1 + b32*x2 + b33*x3 + b34*x4 + b35*x5 + b36*x6))"

g.healthy <- paste("1 / (1 + ", scab, "+", sprout, ")")

g.healthy

calc.healthy <- car::deltaMethod(object =  mod.fit, g = g.healthy,
                          parameterNames = c("b20", "b21", "b22", "b23", "b24", "b25", "b26",
                                             "b30", "b31", "b32", "b33", "b34", "b35", "b36"))
calc.healthy$Estimate  # pi^_Healthy
calc.healthy$SE        # sqrt(Var^(pi^_Healthy))

alpha <- 0.05

calc.healthy$Estimate + qnorm(p = c(alpha/2, 1-alpha/2)) * calc.healthy$SE



# ----------
# pi.hat_Scab
g.scab <- paste(scab, "/ (1 + ", scab, "+", sprout, ")")

calc.scab <- car::deltaMethod(object =  mod.fit, g = g.scab,
                       parameterNames = c("b20", "b21", "b22", "b23", "b24", "b25", "b26",
                                          "b30", "b31", "b32", "b33", "b34", "b35", "b36"))
calc.scab

calc.scab$Estimate + qnorm(p = c(alpha/2, 1-alpha/2)) * calc.scab$SE


# -->
# Notice that these types of intervals can be below 0 or above 1



# ----------
# pi.hat_Sprout
g.sprout <- paste(sprout, "/ (1 + ", scab, "+", sprout, ")")

calc.sprout <- car::deltaMethod(object =  mod.fit, g = g.sprout,
                         parameterNames = c("b20", "b21", "b22", "b23", "b24", "b25", "b26",
                                            "b30", "b31", "b32", "b33", "b34", "b35", "b36"))

calc.sprout$Estimate + qnorm(p = c(alpha/2, 1-alpha/2)) * calc.sprout$SE

