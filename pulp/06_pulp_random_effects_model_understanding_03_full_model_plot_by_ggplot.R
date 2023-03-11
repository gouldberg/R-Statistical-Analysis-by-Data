setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ----------
lmod <- aov(bright ~ operator, data = pulp)

mmod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = TRUE)

mod_obj <- lmod



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

# pulp.fitp <- cbind(pulp, math_pred = predict(mod_obj, type = "response"))

pulp.fitp <- fortify(mod_obj)

head(pulp.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(pulp.fitp, aes(x = operator, y = .fitted)) + theme_bw() + geom_point(colour = "black", size = 1.5)

gg
