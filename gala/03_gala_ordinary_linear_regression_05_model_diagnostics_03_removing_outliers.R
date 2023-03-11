]setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)




# ------------------------------------------------------------------------------
# removing "Isabela" and refit
# ------------------------------------------------------------------------------

lmod_r <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala, subset = (row.names(gala) != "Isabela"))


summary(lmod_r)



# -->
# now the Area is significant




# ----------
car::residualPlots(lmod_r)




# ----------
infl_r <- influence.measures(lmod_r)


summary(infl_r)




# ----------
graphics.off()

car::avPlots(lmod_r, id = TRUE, pch = 20, cex = 1.2, cex.lab = 1.5)


