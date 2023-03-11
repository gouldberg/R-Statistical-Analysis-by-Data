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



# ----------
? vegan::decostand



# ------------------------------------------------------------------------------
# Ecological Data Transformation
#  - Compare transformations of abundances
# ------------------------------------------------------------------------------

# Boxplot:  Compare transformations of abundances of a common species (the stone loach, species #4)

graphics.off();  par(mfrow=c(2,2));

boxplot(spe$Babl, sqrt(spe$Babl), log1p(spe$Babl), las = 1, main = "Simple transformations", names = c("raw data", "sqrt", "log"), col = "bisque")

boxplot(spe.scal$Babl, spe.relsp$Babl, las = 1, main = "Standardization by species", names = c("max", "total"), col = "lightgreen")

boxplot(spe.hel$Babl, spe.rel$Babl, spe.norm$Babl, las = 1, main = "Standardization by sites", names = c("Hellinger", "total", "norm"), col = "lightblue")

boxplot(spe.chi$Babl, spe.wis$Babl, las = 1, main = "Double Standardizations", names = c("Chi-square", "Wisconsin"), col = "orange")



# ------------------------------------------------------------------------------
# Plot raw and transformed abundances along the upstream-downstream river gradient
# ------------------------------------------------------------------------------

graphics.off();  par(mfrow = c(2, 2));
plot(env$dfs, spe$Satr, type = "l", col = 4, main = "Raw data", xlab = "Distance from the source [km]",  ylab = "Raw abundance code")
lines(env$dfs, spe$Thth, col = 3)
lines(env$dfs, spe$Baba, col = "orange")
lines(env$dfs, spe$Abbr, col = 2)
lines(env$dfs, spe$Babl, col = 1, lty = "dotted")

plot(env$dfs, spe.scal$Satr, type = "l", col = 4, main = "Species abundances ranged by maximum", xlab = "Distance from the source [km]",  ylab = "Ranged abundance")
lines(env$dfs, spe.scal$Thth, col = 3)
lines(env$dfs, spe.scal$Baba, col = "orange")
lines(env$dfs, spe.scal$Abbr, col = 2)
lines(env$dfs, spe.scal$Babl, col = 1, lty = "dotted")

plot(env$dfs, spe.hel$Satr, type = "l", col = 4, main =  "Hellinger-transformed abundances", xlab = "Distance from the source [km]", ylab = "Standardized abundance")
lines(env$dfs, spe.hel$Thth, col = 3)
lines(env$dfs, spe.hel$Baba, col = "orange")
lines(env$dfs, spe.hel$Abbr, col = 2)
lines(env$dfs, spe.hel$Babl, col = 1, lty = "dotted")

plot(env$dfs, spe.chi$Satr, type = "l", col = 4, main = "Chi-square-transformed abundances", xlab = "Distance from the source [km]", ylab = "Standardized abundance")
lines(env$dfs, spe.chi$Thth, col = 3)
lines(env$dfs, spe.chi$Baba, col = "orange")
lines(env$dfs, spe.chi$Abbr, col = 2)
lines(env$dfs, spe.chi$Babl, col = 1, lty = "dotted")
# legend("topright", c("Brown trout", "Grayling", "Barbel", "Common bream", "Stone loach"), col = c(4, 3, "orange", 2, 1), lty = c(rep(1, 4), 3))

