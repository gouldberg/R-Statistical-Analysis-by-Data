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
# Identify influential observations:  all at once by influence.measures
# ------------------------------------------------------------------------------

infl <- influence.measures(lmod)

summary(infl)




# ------------------------------------------------------------------------------
# Identify influential observations:  by car package
# ------------------------------------------------------------------------------

car::influenceIndexPlot(lmod, vars = c("studentized"))



# ----------
op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
res <- car::influencePlot(lmod, scale = 8)
k <- length(coef(lmod))
n <- nrow(gala)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)


# show data together with diagnostics for influential cases
idx <- which(rownames(gala) %in% rownames(res))

cbind(gala[idx,], res)




# ----------
car::influenceIndexPlot(lmod, vars = c("Cook", "studentized", "hat"), id.n = 4)


