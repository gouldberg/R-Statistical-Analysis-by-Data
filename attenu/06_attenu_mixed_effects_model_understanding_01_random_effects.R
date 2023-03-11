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
# Model understanding:  station effect
# ------------------------------------------------------------------------------

# These represent station effect
ref <- ranef(mod_obj)$station[[1]]

summary(ref)



# -->
# The difference between the best and the worst is about 0.065.



densityPlot(ref)



# ------------------------------------------------------------------------------
# Model understanding:  event effect
# ------------------------------------------------------------------------------

# These represent event effect
ref <- ranef(mod_obj)$event[[1]]

summary(ref)



# -->
# The difference between the best and the worst is about 0.67.



densityPlot(ref)


