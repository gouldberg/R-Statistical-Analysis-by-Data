setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X
# ------------------------------------------------------------------------------


# jitter is necessary !!
plot(jitter(articles + 1) ~ mentor, data = PhdPubs, log = "y", ylab = "log(articles + 1)", cex.lab = 1.25)


lines(lowess(PhdPubs$mentor, PhdPubs$articles + 1), col = "blue", lwd = 3)



# -->
# The relationship between log(articles) and mentor publications seems largely linear except possibly at the very low end.
# The large number of zero counts at the lower left corner stands out;
# this would not be seen withoug jittering



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------


# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line of log(articles) on mentor publications
ggplot(PhdPubs, aes(mentor, articles + 1)) + geom_jitter(position = position_jitter(h = 0.05)) +
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "log(articles + 1)", x = "Mentor publications")




# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  (discretized)
# ------------------------------------------------------------------------------

# discretize continuous variables
# cutfac():  by default chooses convenient breaks among the values based on deciles
par(mfrow = c(1,1))
plot(articles + 1 ~ vcdExtra::cutfac(mentor), data = PhdPubs, log="y", ylab = "log(articles+1)", xlab = "mentor(deciles)", las=1)




# ------------------------------------------------------------------------------
# data exploration:  log Y (only positive values) vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

PhdPubsPos <- subset(PhdPubs, articles > 0)


ggplot(PhdPubsPos, aes(mentor, articles + 1)) + geom_jitter(position = position_jitter(h = 0.05)) +
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "log(articles + 1)", x = "Mentor publications")




