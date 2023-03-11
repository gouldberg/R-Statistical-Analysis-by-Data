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
# Linear regression
# ------------------------------------------------------------------------------


mod_l <- lm(deaths ~ min_pressure + damage_norm + femininity, data = data)


summary(mod_l)




# ----------
par(mfrow = c(2,2))

plot(mod_l)




# ----------
car::residualPlots(mod_l)



