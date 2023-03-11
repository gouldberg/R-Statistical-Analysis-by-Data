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
mmod <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu, REML = FALSE)


mod_obj <- mmod



# ------------------------------------------------------------------------------
# Model disgnostics:  Check the assumption of normality distributed random effects
# ------------------------------------------------------------------------------

qqnorm(ranef(mod_obj)$station[[1]], main = "station effects")

qqline(ranef(mod_obj)$station[[1]])


# ----------
qqnorm(ranef(mod_obj)$event[[1]], main = "event effects")

qqline(ranef(mod_obj)$event[[1]])
