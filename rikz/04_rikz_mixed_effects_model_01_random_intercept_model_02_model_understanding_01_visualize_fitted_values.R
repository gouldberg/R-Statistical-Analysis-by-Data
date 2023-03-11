# setwd("//media//kswada//MyFiles//R//rikz")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//rikz")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
# ------------------------------------------------------------------------------

RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)


str(RIKZ)


car::some(RIKZ)



# ----------
RIKZ$fBeach <- factor(RIKZ$Beach)

Mlme1 <- lme(Richness ~ NAP, random = ~1 | fBeach, data = RIKZ)

summary(Mlme1)



mod_obj <- Mlme1



# ------------------------------------------------------------------------------
# Visualize random intercept model
# ------------------------------------------------------------------------------

# level = 0:  we take the fitted values obtained by the population model
# level = 1: give the within-beach fitted values.

F0 <- fitted(mod_obj, level = 0)

F1 <- fitted(mod_obj, level = 1)

I <- order(RIKZ$NAP)

NAPs <- sort(RIKZ$NAP)

plot(NAPs, F0[I], lwd = 4, type = "l", ylim=c(0,22), ylab="Richness", xlab="NAP")


for (i in 1:9){
  x1 <- RIKZ$NAP[RIKZ$Beach==i]
  y1 <- F1[RIKZ$Beach==i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}

text(RIKZ$NAP, RIKZ$Richness, RIKZ$Beach, cex=0.9)



# -->
# The thick line represents the fitted line obtained by the fixed component 6.58 - 2.56 * NAP(i), aosl called population model.
# The other lines are obtained by adding the contribution of beta(i) for each beach i to the population fitted curve.
# Hence, the random intercept model implies one average curcu (the thick line) that is allowed to be shifted up, or down,
# for each beach by something that is normally distributed with a certain variance (= 2.94^2)



# ------------------------------------------------------------------------------
# Effect plot
# ------------------------------------------------------------------------------

library(effects)

# but this shows only population model
plot(predictorEffects(mod_obj))


