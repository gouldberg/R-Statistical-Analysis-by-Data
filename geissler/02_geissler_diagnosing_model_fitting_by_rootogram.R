setwd("//media//kswada//MyFiles//R//geissler")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Geissler
#  - The complete data from Geissler's (1889) tabulation of family sex composition in Saxony.
# ------------------------------------------------------------------------------
data("Geissler", package = "vcdExtra")

data <- Geissler

str(data)


# extract only boys in family with size == 11
data <- data %>% filter(size == 11) %>% dplyr::select(boys, Freq)
data <- xtabs(Freq ~ boys, data = data)



# ------------------------------------------------------------------------------
# Fit the binomial and negative binomial distribution to Geissler data
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "binomial")
data_fit1 <- vcd::goodfit(data, type = "nbinomial", par=list(size=11))



# ------------------------------------------------------------------------------
# Rootogram
#
# plot.goodfit() method for "goodfit" objects
# ------------------------------------------------------------------------------
plot(data_fit, type = "hanging", shade = TRUE, xlab = "Number of male children")

plot(data_fit, type = "deviation", shade = TRUE, xlab = "Number of male children")


plot(data_fit1, type = "hanging", shade = TRUE, xlab = "Number of male children")

plot(data_fit1, type = "deviation", shade = TRUE, xlab = "Number of male children")





