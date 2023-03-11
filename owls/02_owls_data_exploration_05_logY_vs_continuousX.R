# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X
# ------------------------------------------------------------------------------

# we apply log2 not log10

# plot(NegPerChick ~ ArrivalTime, data = Owls, log10 = "y", ylab = "NegPerChick", cex.lab = 1.25, col = gray(0.7), pch = 20)
plot(NegPerChick ~ ArrivalTime, data = Owls, log = "y", ylab = "NegPerChick", cex.lab = 1.25, col = gray(0.7), pch = 20)

lines(lowess(Owls$ArrivalTime, Owls$NegPerChick), col = "blue", lwd = 3)



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------


# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line

ggplot(Owls, aes(ArrivalTime, NegPerChick)) + geom_jitter(position = position_jitter(h = 0.05)) +
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "log10(NegPerChick)", x = "ArrivalTime")




# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  (discretized)
# ------------------------------------------------------------------------------

# discretize continuous variables
# cutfac():  by default chooses convenient breaks among the values based on deciles

par(mfrow = c(1,1))
plot(NegPerChick + 0.1 ~ vcdExtra::cutfac(ArrivalTime), data = Owls, log="y", ylab = "log(NegPerChick)", xlab = "ArrivalTime", las=1)



