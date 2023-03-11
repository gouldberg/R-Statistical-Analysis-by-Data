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
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)


# Baltra deleted
lmod_del <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala[-1,])




# ------------------------------------------------------------------------------
# Finding jointly influential observations
# ------------------------------------------------------------------------------

graphics.off()


car::avPlots(lmod, id = TRUE, pch = 20, cex = 1.2, cex.lab = 1.5)




# -->
# Santa Cruz and Isabela are suspicious


# -->
# the labeled points:  either large absolute model residuals or extreme x residuals, given all other predictors. 
# the solid red line:  partial slope beta for the focal predictor, controlling for all others.




# ------------------------------------------------------------------------------
# Check for Area
# ------------------------------------------------------------------------------

# Area | Others,  Species | Others

lmod_area <- lm(Area ~ Elevation + Nearest + Scruz + Adjacent, data = gala)

lmod_spe <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, data = gala)



# residuals for Isabella

resid(lmod_area)[16]

resid(lmod_spe)[16]




# ----------
# excluding Isabella and SantaCruz and refit

lmod2 <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala[c(-16,-25),])


coef(lmod)

coef(lmod2)





# ------------------------------------------------------------------------------
# Check for Adjacent
# ------------------------------------------------------------------------------

# Adjacent | Others,  Species | Others

lmod_adj <- lm(Adjacent ~ Area + Elevation + Nearest + Scruz, data = gala)

lmod_spe <- lm(Species ~ Area + Elevation + Nearest + Scruz, data = gala)



# residuals for Fernandina

resid(lmod_adj)[12]

resid(lmod_spe)[12]




# ----------
# excluding Fernandina and SantaCruz

lmod2 <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala[c(-12,-25),])


coef(lmod)

coef(lmod2)

