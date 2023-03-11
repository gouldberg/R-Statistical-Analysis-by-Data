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




# ----------
library(MASS)

( nbin <- glm.nb(deaths ~ min_pressure + damage_norm + femininity, data = data) )

( theta <- nbin$theta )

mod.nbin <- glm(deaths ~ min_pressure + damage_norm + femininity, data = data, family = negative.binomial(theta))





# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)


mod_obj <- mod.nbin



plot(Effect(c("femininity", "min_pressure"), mod_obj), 
#     confint = list(style = "bands"),
     confint = list(style = "bars"),
     lines = list(multiline = TRUE, lty = c(1,2,3,4,5), col = c("black", gray(0.2), gray(0.4), gray(0.6), gray(0.8))))




# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------


mod_obj <- mod.nbin



fitp <- cbind(mod_obj$model, pred = predict(mod_obj))

head(fitp)



library(ggplot2)

graphics.off()

gg1 <- ggplot(fitp, aes(x = min_pressure, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg1 <- ggplot(fitp, aes(x = femininity, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()


gg1



