setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "RgoogleMaps", "googleVis", "gclus")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



source("./functions/panelutils.R")



# ------------------------------------------------------------------------------
# Site - Environmental data:  mapped on spatial map 
# ------------------------------------------------------------------------------

func_plot <- function(x){
  plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = 5 * x / max(x), main = colnames(x), xlab = "x coordinate (km)", ylab = "y coordinate (km)")
  lines(spa, col = "light blue")
}



# ----------
graphics.off()
par(mfrow = c(2,2))

obj <- c(1,2,3,4)
apply(env[,obj], 2, func_plot)




# ------------------------------------------------------------------------------
# Bubble maps of 4 environmental variables
# ------------------------------------------------------------------------------

# Mapping on nit (Nitrate) reveals that nit has specific sites at middle of downstream where the its value is large
par(mfrow = c(2,2))

plot(spa, asp = 1, cex.axis = 0.8, main = "Elevation", pch = 21, col = "white", bg = "red", cex = 5 * env$ele / max(env$ele), xlab = "x", ylab = "y")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, main = "Mean minimum discharge", pch = 21, col = "white", bg = "blue", cex = 5 * env$dis / max(env$dis), xlab = "x", ylab = "y")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, main = "Dissolved oxygen", pch = 21, col = "white", bg = "green3", cex = 5 * env$oxy / max(env$oxy), xlab = "x", ylab = "y")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, main = "Nitrate concentration", pch = 21, col = "white", bg = "brown", cex = 5 * env$nit / max(env$nit), xlab = "x", ylab = "y")
lines(spa, col = "light blue")



# -->
# Mapping on nit (Nitrate) reveals that nit has specific sites at middle of downstream where the its value is large



# ----------
env.ken <- cor(env, method = "kendall")
env.o <- gclus::order.single(env.ken)
round(cor(env[,env.o], method = "kendall"), digits = 3)



# ------------------------------------------------------------------------------
# Bubble maps of other environmental variables
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(spa, asp = 1, cex.axis = 0.8, main = "Slope", pch = 21, col = "white", bg = "red", cex = 5 * env$slo / max(env$slo), xlab = "x", ylab = "y")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, main = "pH of water", pch = 21, col = "white", bg = "blue", cex = 5 * env$pH / max(env$pH), xlab = "x", ylab = "y")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, main = "Hardness (Ca concentration)", pch = 21, col = "white", bg = "green3", cex = 5 * env$har / max(env$har), xlab = "x", ylab = "y")
lines(spa, col = "light blue")



# ----------
# Mapping on amm (Ammmonium concentration), pho (Phosphate concentration) and bod (Biological oxygen demand) reveals that
# those specific sites at middle of downstream where the its value is large
par(mfrow = c(2,2))

plot(spa, asp = 1, cex.axis = 0.8, main = "Nitrate concentration", pch = 21, col = "white", bg = "brown", cex = 5 * env$nit / max(env$nit), xlab = "x", ylab = "y")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, main = "Ammonium concentration", pch = 21, col = "white", bg = "brown", cex = 5 * env$amm / max(env$amm), xlab = "x", ylab = "y")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, main = "Phosphate concentration", pch = 21, col = "white", bg = "red", cex = 5 * env$pho / max(env$pho), xlab = "x", ylab = "y")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, main = "Biological oxygen demand", pch = 21, col = "white", bg = "blue", cex = 5 * env$bod / max(env$bod), xlab = "x", ylab = "y")
lines(spa, col = "light blue")



# ------------------------------------------------------------------------------
# Site - Environmental data:  Examine the variation of environmental variables along the river
# ------------------------------------------------------------------------------

func_plot <- function(x){
  plot(env$dfs, x, type = "l", xlab = "Distance from the source (km)", col = "red")
}


graphics.off()
par(mfrow = c(2,2))
obj <- c(1,2,3,4)
apply(env[,obj], 2, func_plot)



# ----------
# Line plots of environmental variables
par(mfrow = c(2,2))
plot(env$dfs, env$ele, type = "l", xlab = "Distance from the source (km)", ylab = "Elevation (m)", col = "red", main = "Elevation")
plot(env$dfs, env$dis, type = "l", xlab = "Distance from the source (km)", ylab = "Discharge (m3/s)", col = "blue", main = "Discharge")
plot(env$dfs, env$oxy, type = "l", xlab = "Distance from the source (km)", ylab = "Oxygen (mg/L)", col = "green3", main = "Oxygen")
plot(env$dfs, env$nit, type = "l", xlab = "Distance from the source (km)", ylab = "Nitrate (mg/L)", col = "brown", main = "Nitrate")

par(mfrow = c(2,2))
plot(env$dfs, env$slo, type = "l", xlab = "Distance from the source (km)", ylab = "Slope", col = "red", main = "Slope")
plot(env$dfs, env$pH, type = "l", xlab = "Distance from the source (km)", ylab = "pH", col = "blue", main = "pH")
plot(env$dfs, env$har, type = "l", xlab = "Distance from the source (km)", ylab = "Hardness (mg/L)", col = "green3", main = "Hardness")
plot(env$dfs, env$pho, type = "l", xlab = "Distance from the source (km)", ylab = "Phosphate (mg/L)", col = "brown", main = "Phosphate")

par(mfrow = c(2,2))
plot(env$dfs, env$amm, type = "l", xlab = "Distance from the source (km)", ylab = "Ammmonium", col = "red", main = "Ammonium")
plot(env$dfs, env$bod, type = "l", xlab = "Distance from the source (km)", ylab = "BOD", col = "blue", main = "Biological Oxygen Demand")


# "oxy" has negative correlation with nit, pho, amm and bod
par(mfrow = c(3,2))
plot(env$dfs, env$oxy, type = "l", xlab = "Distance from the source (km)", ylab = "Oxygen (mg/L)", col = "green3", main = "Oxygen")
plot(env$dfs, env$nit, type = "l", xlab = "Distance from the source (km)", ylab = "Nitrate (mg/L)", col = "brown", main = "Nitrate")
plot(env$dfs, env$pho, type = "l", xlab = "Distance from the source (km)", ylab = "Phosphate (mg/L)", col = "brown", main = "Phosphate")
plot(env$dfs, env$amm, type = "l", xlab = "Distance from the source (km)", ylab = "Ammmonium", col = "red", main = "Ammonium")
plot(env$dfs, env$bod, type = "l", xlab = "Distance from the source (km)", ylab = "BOD", col = "blue", main = "Biological Oxygen Demand")


