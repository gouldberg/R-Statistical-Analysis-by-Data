setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\hurricanes")

packages <- c("dplyr")

purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hurricanes
# ------------------------------------------------------------------------------

# data("Hurricanes", package = "rethinking")

data <- read.csv(file = "Hurricanes.txt", header = T, sep = "\t")


dim(data)


str(data)



car::some(data)




# ------------------------------------------------------------------------------
# negative-binomial model
# ------------------------------------------------------------------------------

library(MASS)


# estimate theta first
( nbin <- glm.nb(deaths ~ min_pressure + damage_norm + femininity, data = data) )


( theta <- nbin$theta )



# -->
# dispersion parameter is 0.7174
# Far better than Poisson model




# ----------
mod.nbin <- glm(deaths ~ min_pressure + damage_norm + femininity, data = data, family = negative.binomial(theta))


summary(mod.nbin)




# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------

car::compareCoefs(mod_l, mod_p, mod.nbin)


AIC(mod_l, mod_p, mod.nbin)



