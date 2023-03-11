setwd("//media//kswada//MyFiles//R//saxony")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Saxnony
#  - The nmber of male children in families of size 12 in Saxony
# ------------------------------------------------------------------------------
data("Saxony", package = "vcd")

data <- Saxony



# ------------------------------------------------------------------------------
# barplot
# ------------------------------------------------------------------------------
k = names(data)

par(mfrow=c(1,1))
b <- barplot(data, names.arg = k, xlab = "Number of male children in families of size 12 in Saxony", ylab = "Frequency")
lines(x = b, y = data, col = "red")



# ------------------------------------------------------------------------------
# Fit the binomial distribution to Saxony data
#  - estimating parameter by maximum likelihood estimation
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "binomial")



# ----------
# estimated paramters by maximum likelihood estimation
# the probability of a male in these families to be p = 0.519, a value that is quite close to the value found in Arbuthnot's data (p = 0.517)
unlist(data_fit$par)



# ----------
data_fit



# ----------
summary(data_fit)



# ------------------------------------------------------------------------------
# Fit the binomial distribution to Saxony data by specifying paramters
# ------------------------------------------------------------------------------
# Fit Bin(n=12, p=0.5) to this data
data_fit1 <- vcd::goodfit(data, type = "binomial", par=list(size = 12, prob = 1/2))



# ----------
# Goodness of Fit test
# summary(data_fit1) does not have X^2/df statistic, so need to calculate by yourself
data_df1 <- as.data.frame(data)
k <- 0:12

Exp1 <- data_fit1$fitted
Diff1 <- data_fit1$observed - Exp1
Chisq1 <- Diff1^2 / Exp1

data_df1 <- data.frame(data_df1, Exp1, Diff1, Chisq1)
data_df1


# chisq --> 249.1954
sum(Chisq1)


# X^2/df:  df is nrow(data) - 1  --> 20.76629
sum(Chisq1) / 12


# The binomial model Bin(n=12, p=0.5) fits worse than the model with paratmer estimated by ML
summary(data_fit)
pchisq(sum(Chisq1), df = nrow(data) - 1, lower.tail = FALSE)



# ------------------------------------------------------------------------------
# Fit the binomial distribution to Saxony data by specifying paramters estimated by ML
# ------------------------------------------------------------------------------
# Fit Bin(n=12, p=0.519) to this data
data_fit2 <- vcd::goodfit(data, type = "binomial", par=list(size = 12, prob = unlist(data_fit$par["prob"])))



# ----------
# Goodness of Fit test
# summary(data_fit1) does not have X^2/df statistic, so need to calculate by yourself
data_df2 <- as.data.frame(data)
k <- 0:12

Exp2 <- data_fit2$fitted
Diff2 <- data_fit2$observed - Exp2
Chisq2 <- Diff2^2 / Exp2

data_df2 <- data.frame(data_df2, Exp2, Diff2, Chisq2)
data_df2


# chisq --> 110.505
sum(Chisq2)


# X^2/df:  df is nrow(data) - 2  --> 10.04591
sum(Chisq2) / 11


# Compare the result
summary(data_fit)
pchisq(sum(Chisq1), df = nrow(data) - 1, lower.tail = FALSE)
pchisq(sum(Chisq2), df = nrow(data) - 2, lower.tail = FALSE)
