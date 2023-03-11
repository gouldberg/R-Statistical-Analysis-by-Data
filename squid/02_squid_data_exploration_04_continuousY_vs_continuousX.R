# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)


# ----------
Squid$fMONTH = factor(Squid$MONTH)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(Testisweight ~ DML, data = Squid, log = c("x", "y"), ylab = "Testisweight", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(Squid$DML, Squid$Testisweight), col = "blue", lwd = 1)


plot(Specimen ~ DML, data = Squid, log = c("x", "y"), ylab = "Specimen", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(Squid$DML, Squid$Specimen), col = "blue", lwd = 1)




# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line of log(articles) on mentor publications
gg <- ggplot(Squid, aes(DML, Testisweight)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "Testisweight", x = "DML")


gg



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  (discretized)
# ------------------------------------------------------------------------------

# discretize continuous variables
# cutfac():  by default chooses convenient breaks among the values based on deciles
par(mfrow = c(1,1))

# plot(Testisweight ~ vcdExtra::cutfac(DML, 10), data = Squid, ylab = "Testisweight", xlab = "DML", las=1)
plot(Testisweight ~ cut(DML, 10), data = Squid, ylab = "Testisweight", xlab = "DML", las=1)

