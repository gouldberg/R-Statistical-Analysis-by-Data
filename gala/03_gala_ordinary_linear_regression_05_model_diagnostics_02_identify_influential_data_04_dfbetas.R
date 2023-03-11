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
# Identify influential observations:  dfbetas
# ------------------------------------------------------------------------------

dfbeta(lmod)




# ----------
# for Baltra

dfbeta(lmod)[1,]


coef(lmod) - coef(lmod_del)




# -->
# positive value means coefficient is decreased, meaning Baltra has positive effect for the coefficient




# ----------
crit <- 0.25

abs(dfbeta(lmod)) >= crit

( abs(dfbeta(lmod)) >= crit ) * 100




# ----------
dfbeta(lmod)["Isabela",]

gala["Isabela",]


par(mfrow = c(1,2))

plot(Species ~ Area, data = gala)
text(gala$Area, gala$Species, labels = rownames(gala), cex = 0.8)


plot(Species ~ Nearest, data = gala)
text(gala$Nearest, gala$Species, labels = rownames(gala), cex = 0.8)




# ------------------------------------------------------------------------------
# ScatterplotMatrix of dfbetas:  Joint dfbetas
# ------------------------------------------------------------------------------


( tmp <- data.frame(dfbeta(lmod)) )


formula <- ~ Area + Elevation + Nearest + Adjacent

car::scatterplotMatrix(formula, data = tmp,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)



#car::scatterplotMatrix(tmp[,-1], smooth = FALSE, id = TRUE, showLabels=list(method = "mahal", n = 2, cex = 1.2, location = "lr"), 
#                       ellipse = TRUE, levels = 0.95, robust = FALSE, diagonal = "histogram", col = gray(0.6))





# ----------
# all logged data

lmod2 <- lm(log(Species) ~ log(Area) + log(Elevation) + log(Nearest) + log(Scruz + 0.001) + log(Adjacent), data = gala)


( tmp <- data.frame(dfbeta(lmod2)) )


formula <- ~ log.Area. + log.Elevation. + log.Nearest. + log.Adjacent.

car::scatterplotMatrix(formula, data = tmp,
                       smooth = FALSE,
                       id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)

