# setwd("//media//kswada//MyFiles//R//rikz")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//rikz")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
# ------------------------------------------------------------------------------

RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)


str(RIKZ)


car::some(RIKZ)



# ------------------------------------------------------------------------------
# Fit linear model by each person by lmList()
# ------------------------------------------------------------------------------

library(lme4)


# We now fit a line for all the subjects and plot the results.

# This produces NA for Exposure ...
# ml <- lmList(Richness ~ NAP + Exposure | Beach, RIKZ)


ml <- lmList(Richness ~ NAP | Beach, RIKZ)

ml


summary(ml[[1]])


# -->
# not significant ...


# ----------
# extract intercepts and slopes
intercepts <- sapply(ml, coef)[1,]

slopes <- sapply(ml, coef)[2,]


df <- cbind(RIKZ %>% dplyr::select(Beach) %>% unique() %>% arrange(Beach), intercepts, slopes)

df

