setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ----------
modp <- glm(articles ~ ., data = PhdPubs, family = poisson(link = "log"))



# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)



# effect plots for several predictors jointly or full-model plots
eff2 <- Effect(c("married", "kid5", "female"), modp)


plot(eff2, 
     mutiline = TRUE, ci.stype = "bands",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)),
     key.args = list(x = 0.52, y = 0.92, columns = 1), grid = TRUE)


plot(eff2, 
     mutiline = TRUE, ci.stype = "bands", type = "response",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)),
     key.args = list(x = 0.52, y = 0.92, columns = 1), grid = TRUE)

