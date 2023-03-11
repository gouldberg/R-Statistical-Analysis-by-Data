setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ----------
lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

or.fitp <- cbind(lmod$model, pred = predict(lmod, type = "response"))


head(or.fitp)

colnames(or.fitp) <- c("damage_notdamaged", "temp", "pred")



# ----------
library(ggplot2)

graphics.off()

gg <- ggplot(or.fitp, aes(x = temp, y = pred)) + theme_bw() + geom_line(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg
