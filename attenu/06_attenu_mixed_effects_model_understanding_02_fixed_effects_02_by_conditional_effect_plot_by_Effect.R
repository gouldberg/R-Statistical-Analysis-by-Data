setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)



# ----------
attenu <- na.exclude(attenu)


attenu <- attenu %>% mutate(mag2 = ifelse(mag < 6, 5, ifelse(mag < 7, 6, 7)))



# ----------
mmod1 <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu)
mmod2 <- lmer(accel^0.25 ~ log(dist) + mag + (1 | event), data = attenu)


mod_obj <- mmod2



# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)



# effect plots for several predictors jointly or full-model plots

plot(Effect(c("dist", "mag"), mod_obj), 
     confint = list(style = "bands"),
     lines = list(multiline = TRUE, lty = c(5,4,3,2,1), col = c(gray(0.8), gray(0.6), gray(0.4), gray(0.2), "black")))


