setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X
# ------------------------------------------------------------------------------

# we limit to positive intensity
CodParasitesPos <- subset(CodParasites, prevalence == "yes")


# jitter is necessary !!
plot(jitter(intensity + 1) ~ length, data = CodParasitesPos, log = "y", ylab = "log(intensity + 1)", cex.lab = 1.25)


# na.omit is required
lines(lowess(na.omit(CodParasitesPos)$length, na.omit(CodParasitesPos)$intensity + 1), col = "blue", lwd = 3)



# -->
# For only positive instensity data,
# the relationship between log(intensity) and length seems some linear



# ----------
plot(jitter(intensity + 1) ~ weight, data = CodParasitesPos, log = "y", ylab = "log(intensity + 1)", cex.lab = 1.25)
lines(lowess(na.omit(CodParasitesPos)$weight, na.omit(CodParasitesPos)$intensity + 1), col = "blue", lwd = 3)



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------


# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line of log(articles) on mentor publications

# Note that ggplot can handle missing values automatically
ggplot(CodParasitesPos, aes(length, intensity + 1)) + geom_jitter(position = position_jitter(h = 0.05)) +
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "log(intensity + 1)", x = "Length")



# -->
# For the effect of length of fish,
# the smoothed soess curve together with the linear regression line show no indication of nonlinearity.



# ------------------------------------------------------------------------------
# data exploration:  log Y vs. continuous X  (discretized)
# ------------------------------------------------------------------------------

# discretize continuous variables
# cutfac():  by default chooses convenient breaks among the values based on deciles

# na.omit is required here.
par(mfrow = c(1,1))
plot(intensity + 1 ~ vcdExtra::cutfac(length), data = na.omit(CodParasitesPos), log="y", ylab = "log(intensity+1)", xlab = "length(deciles)", las=1)



