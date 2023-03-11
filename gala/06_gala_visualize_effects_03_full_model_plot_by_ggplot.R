setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
mod_obj <- modp.step2
# mod_obj <- modp2



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

gala.fitp <- cbind(gala, Species_pred = predict(mod_obj, type = "response"))


head(gala.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(gala.fitp, aes(x = Area, y = Species_pred)) + theme_bw() + geom_point(colour = "black", size = 1.5)

gg + facet_grid(~ cutfac(Nearest, q = 5))

