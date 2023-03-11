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
# barplot
# ------------------------------------------------------------------------------
k = names(data)

par(mfrow=c(1,1))
b <- barplot(data, names.arg = k, xlab = "Number of male children in families of size 11 in Saxony", ylab = "Frequency")
lines(x = b, y = data, col = "red")



# ------------------------------------------------------------------------------
# Fit the binomial distribution to Geissler data
#  - estimating parameter by maximum likelihood estimation
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "binomial")


# ----------
# estimated paramters by maximum likelihood estimation
# the probability of a male in these families to be p = 0.5170763
unlist(data_fit$par)



# ----------
data_fit



# ----------
# It indicate that the binomial does not fit these data.
summary(data_fit)



# ------------------------------------------------------------------------------
# Fit the negative binomial distribution to Geissler data
#  - estimating parameter by maximum likelihood estimation
# ------------------------------------------------------------------------------
data_fit1 <- vcd::goodfit(data, type = "nbinomial", par=list(size=11))

data_fit1

summary(data_fit1)

