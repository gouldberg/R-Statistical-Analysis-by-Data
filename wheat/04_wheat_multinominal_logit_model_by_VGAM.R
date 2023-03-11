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
# multinominal logit model by VGAM
# ------------------------------------------------------------------------------

library(VGAM)

# This will match mod.fit with multinom()
mod.fit2 <- vglm(formula = type ~ class + density + hardness + size + weight + moisture, family = multinomial(refLevel = 1), data = wheat)


# The estimated coefficients are same
# but the output from VGAM model shows p-values and other information
summary(mod.fit2)

summary(mod.fit)




# ----------
# estimated values (type = "link")
save.pred <- predictvglm(object = mod.fit2, newdata = wheat[1,], type = "link", se.fit = TRUE)
# predictvglm(object = mod.fit2, newdata = wheat[1,], type = "response", se.fit = TRUE)  # Not allowed

save.pred
