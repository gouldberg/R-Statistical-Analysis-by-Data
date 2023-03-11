setwd("//media//kswada//MyFiles//R//visualization")

packages <- c("dplyr", "vcd", "MASS", "lattice", "directlabels", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Poisson distribution
#
#   -  The Poisson distribution give the probability of an event occurring k = 0, 1, 2, ... times over a large number of independent trials,
#      when the probability, p, that the event occurs on any one trial (in time or space) is small and constant.
#   -  The Poisson distribution is usually applied to the study of rare events such as highway accidents ata particular location, deaths from horse kicks,
#      or defects in a well-controlled manufacturing process.
#   -  Other applications include:
#        - the number of customers contacting a call center per unit time
#        - the number of insurance claims per unit region or unit time
#        - number of particles emitted from a small radioactive sample
#
#   -  The mean and variance of the Poisson distribution are always the same, which is sometimes used to identify a distribution as Poisson.
#      (fot the binomial distribution, the mean is always greater than the variance, and for negative binomial and geometric distribution, the mean
#      is less than the variance)
#   -  The Poisson distribution is always positively skewed, but skewness decreases as lambda increases.
#   -  
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# plot Poisson distribution in multi-panel display:  lattice::xyplot()
# ------------------------------------------------------------------------------
lambda <- c(1, 4, 10)
k <- 0:20


KL <- expand.grid(k = k, lambda = c(1,4,10))

pois_df <- data.frame(KL, prob = dpois(KL$k, KL$lambda))

pois_df$lambda <- factor(pois_df$lambda)

str(pois_df)


lattice::xyplot(prob ~ k | lambda, data = pois_df,
       xlab = list("Number of events (k)", cex = 1.25), ylab = list("Probability", cex = 1.25), type = c("h", "p"), pch = 16, lwd = 4,
       cex = 1.25, layout = c(3,1))



# ------------------------------------------------------------------------------
# plot Poisson distribution using direct labels
# ------------------------------------------------------------------------------
mycol <- palette()[2:4]


# Note that the plot constructed by xyplot() is save as a ("trellis") object, plt.
# The function direct.label() messages this to add the labels directly to each curve.
# In the second argument above, "top.points" says to locate these at the maximum value on each curve.
plt <- xyplot(prob ~ k, data = pois_df, groups = lambda,
              xlab = list("Number of Events (k)", cex = 1.25), ylab = list("Probability", cex = 1.25),
              type = "b", pch = 15:17, lwd = 2, cex = 1.25, col = mycol, ylim = c(0, 0.4))

direct.label(plt, list("top.points", cex = 1.5, dl.trans(y = y + 0.1)))



# ------------------------------------------------------------------------------
# plot Poisson distribution using ggplot2
# ------------------------------------------------------------------------------
gplt <- ggplot(pois_df, aes(x = k, y = prob, colour = lambda, shape = lambda)) + 
  geom_line(size = 1) + geom_point(size = 3) +
  xlab("Number of events (k)") +
  ylab("Probability")


gplt + theme(legend.position = c(0.8, 0.8)) + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"))



