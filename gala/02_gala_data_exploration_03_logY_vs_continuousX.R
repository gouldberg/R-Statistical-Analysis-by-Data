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




# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X
# ------------------------------------------------------------------------------


# jitter is necessary !!
par(mfrow = c(2,2))

plot(jitter(Species + 1) ~ Endemics, data = gala, log = c("x", "y"), ylab = "log(Species)", cex.lab = 1.25)
lines(lowess(gala$Endemics, gala$Species + 1), col = "blue", lwd = 1)


plot(jitter(Species + 1) ~ Elevation, data = gala, log = c("x", "y"), ylab = "log(Species)", cex.lab = 1.25)
lines(lowess(gala$Elevation, gala$Species + 1), col = "blue", lwd = 1)


plot(jitter(Species + 1) ~ Area, data = gala, log = c("x", "y"), ylab = "log(Species)", cex.lab = 1.25)
lines(lowess(gala$Area, gala$Species + 1), col = "blue", lwd = 1)



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------


library(ggplot2)

# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line of log(articles) on mentor publications
ggplot(gala, aes(Endemics, Species + 1)) + geom_jitter(position = position_jitter(h = 0.05)) +
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "log(Species + 1)", x = "Endemics")



ggplot(gala, aes(Elevation, Species + 1)) + geom_jitter(position = position_jitter(h = 0.05)) +
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "log(Species + 1)", x = "Elevation")




# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  (discretized)
# ------------------------------------------------------------------------------

# discretize continuous variables
# cutfac():  by default chooses convenient breaks among the values based on deciles
par(mfrow = c(1,1))
plot(Species + 1 ~ vcdExtra::cutfac(Endemics), data = gala, log="y", ylab = "log(Species+1)", xlab = "Endemics(deciles)", las=1)


par(mfrow = c(1,1))
plot(Species + 1 ~ vcdExtra::cutfac(Elevation), data = gala, log="y", ylab = "log(Species+1)", xlab = "Elevation(deciles)", las=1)


par(mfrow = c(1,1))
plot(Species + 1 ~ vcdExtra::cutfac(Area), data = gala, log="y", ylab = "log(Species+1)", xlab = "Area(deciles)", las=1)

