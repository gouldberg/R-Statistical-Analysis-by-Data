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
# data exploration:  Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)



# ----------
# deaths vs. min_pressure
gg <- ggplot(data, aes(min_pressure, deaths)) + geom_point(size = 2, alpha = 0.6) + geom_smooth()

gg


# by group
gg + facet_grid(~ female)




# ----------
# deaths vs. damage_norm
gg <- ggplot(data, aes(damage_norm, deaths)) + geom_point(size = 2, alpha = 0.6) + geom_smooth()

gg


# by group
gg + facet_grid(~ female)



# ----------
# deaths vs. femininity
gg <- ggplot(data, aes(femininity, deaths)) + geom_point(size = 2, alpha = 0.6) + geom_smooth()

gg


# by group
gg + facet_grid(~ female)




# ----------
plot(deaths ~ year, data = data, pch = 2)

