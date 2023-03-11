setwd("//media//kswada//MyFiles//R//debt")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  debt
# ------------------------------------------------------------------------------

data("debt", package = "faraway")


str(debt)


head(debt)



# ------------------------------------------------------------------------------
# Linear Discriminant Analysis
# ------------------------------------------------------------------------------

library(MASS)


mlda0 <- lda(ccarduse ~ ., data = na.omit(debt))


mlda0



# -->
# The means of children, manage, xmasbuy reveal that there is not much difference of the three responses



# ----------
mlda <- update(mlda0, . ~ . - children - manage - xmasbuy)

mlda




# -->
# Trace:  We see that the 1st component is strongly dominant and so the classification will depend mostly on this.

# Coefficients of Linear Discriminants:
# cigbuy has negative coefficient in LD1



# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------

preds <- predict(mlda)

head(preds$posterior)



# ----------
# We can get the most likely outcome from each case and compare it against the observed class.
xtabs(~ predict(mlda)$class + na.omit(debt)$ccarduse)



# -->
# ccarduse = 2 (occasionally) and 3 (regularly) are not good ...



