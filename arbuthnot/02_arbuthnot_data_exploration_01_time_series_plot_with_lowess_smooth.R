setwd("//media//kswada//MyFiles//R//arbuthnot")

packages <- c("dplyr", "vcd", "MASS", "HistData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arbuthnot data
# ------------------------------------------------------------------------------

data("Arbuthnot", package = "HistData")


str(Arbuthnot)


dim(Arbuthnot)


car::some(Arbuthnot)



# ------------------------------------------------------------------------------
# Data Exploration:  time series plot of Male ratio over Year
# ------------------------------------------------------------------------------

with(Arbuthnot, {
  prob = Males / (Males + Females)
  plot(x = Year, y = prob, type = "b", ylim = c(0.5, 0.54), ylab = "Pr(Male)")
  abline(h = 0.5, col = "red", lwd = 2)
  abline(h = mean(prob), col = "blue")
  lines(loess.smooth(Year, prob), col = "blue", lwd = 2)
  text(x = 1640, y = 0.5, expression(H[0]: "Pr(Male)=0.5"), pos = 3, col = "red")
})



# -->
# Loess smoothed curve suggests that any deviation from a constant sex ratio is relatively small, but also shows some systematic trend over time.



# ------------------------------------------------------------------------------
# time series plot of Ratio over Year
# ------------------------------------------------------------------------------

with(Arbuthnot, {
  plot(x = Year, y = Ratio, type = "b", ylim = c(1.00, 1.17), ylab = "Ratio")
  abline(h = 1.0, col = "red", lwd = 2)
  abline(h = mean(Ratio), col = "blue")
  lines(loess.smooth(Year, Ratio), col = "blue", lwd = 2)
})



# ------------------------------------------------------------------------------
# time series plot of Male + Female over Year
# ------------------------------------------------------------------------------

with(Arbuthnot, {
  ttl = Males + Females
  plot(x = Year, y = ttl, type = "b", ylim = c(5000, 17000), ylab = "Males + Females")
  abline(h = 1.0, col = "red", lwd = 2)
  abline(h = mean(ttl), col = "blue")
  lines(loess.smooth(Year, ttl), col = "blue", lwd = 2)
  text(x = 1640, y = mean(ttl), paste0("mean = ", round(mean(ttl),0)), pos = 3, col = "red")
})



# -->
# We can see that total number of christenings (males + females) has negative correlations with Ratio and male ratio

with(data, cor(Males+Females, Ratio), method="spearman")



# ------------------------------------------------------------------------------
# time series plot of Plague and Mortality
# ------------------------------------------------------------------------------

# At year 1664, plague and mortality value is extreme !!

with(Arbuthnot, {
  plot(x = Year, y = Plague, type = "b", ylab = "Plague")
  abline(h = 1.0, col = "red", lwd = 2)
  abline(h = mean(Plague), col = "blue")
  lines(loess.smooth(Year, Plague), col = "blue", lwd = 2)
})



with(Arbuthnot, {
  plot(x = Year, y = Mortality, type = "b", ylab = "Mortality")
  abline(h = 1.0, col = "red", lwd = 2)
  abline(h = mean(Mortality), col = "blue")
  lines(loess.smooth(Year, Mortality), col = "blue", lwd = 2)
})


