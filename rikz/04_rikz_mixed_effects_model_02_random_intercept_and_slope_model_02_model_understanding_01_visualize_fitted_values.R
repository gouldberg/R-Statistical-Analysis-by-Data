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

Mlme2 <- lme(Richness ~ NAP, random = ~1 + NAP | fBeach, data = RIKZ)

summary(Mlme2)


mod_obj <- Mlme2



# ------------------------------------------------------------------------------
# Visualize random intercept and slope model
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

