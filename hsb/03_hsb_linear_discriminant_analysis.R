setwd("//media//kswada//MyFiles//R//hsb")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hsb
# ------------------------------------------------------------------------------

data("hsb", package = "faraway")


str(hsb)


head(hsb)



# ------------------------------------------------------------------------------
# Linear Discriminant Analysis
# ------------------------------------------------------------------------------

library(MASS)


mlda0 <- lda(prog ~ gender + race + ses + schtyp + read + write + math + science + socst, data = hsb)


mlda0



# -->
# The means of the group reveal that there is not much difference in the race of the three choice,



# ----------
mlda <- update(mlda0, . ~ . - race)

mlda




# -->
# Trace:  We see that the 1st component is strongly dominant and so the classification will depend mostly on this.

# Coefficients of Linear Discriminants:
# science has positive coefficients, but other subjects have negative.



# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------

preds <- predict(mlda)

head(preds$posterior)



# ----------
# We can get the most likely outcome from each case and compare it against the observed class.
xtabs(~ predict(mlda)$class + hsb$prog)



# -->
# "general" and "vocation" is not good ..



