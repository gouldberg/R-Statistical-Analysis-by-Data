setwd("//media//kswada//MyFiles//R//women_queue")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Women in queues
#  - Jinkinson and Slater (1981) and Hoaglin and Tukey (1985) give the frequency distribution of the number of females
#    observed in 100 queues of length 10 in a London Underground station
# ------------------------------------------------------------------------------
data("WomenQueue", package = "vcd")

data <- WomenQueue
data


# ------------------------------------------------------------------------------
# barplot
#  - If it is assumed that people line up independently, and that men and women are equally likely to be found in a queue,
#    then the number of women out of 10 would have a (symmetric) binomial distribution with paramters n = 10 and p = 1/2
# ------------------------------------------------------------------------------
k = names(data)

par(mfrow=c(1,1))
b <- barplot(data, names.arg = k, xlab = "Number of females each in queue of length 10 in a London Underground station", ylab = "Frequency")
lines(x = b, y = data, col = "red")



# ------------------------------------------------------------------------------
# Fit the binomial distribution to WomenQueue data
#  - estimating parameter by maximum likelihood estimation
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "binomial", par=list(size=10))


# ----------
# estimated paramters by maximum likelihood estimation
# the probability p = 0.435, which is much less than 0.5
unlist(data_fit$par)



# ----------
data_fit



# ----------
summary(data_fit)



# ------------------------------------------------------------------------------
# Fit the binomial distribution by specifying paramters
# ------------------------------------------------------------------------------
# Fit Bin(n=10, p=0.5) to this data
data_fit1 <- vcd::goodfit(data, type = "binomial", par=list(size = 10, prob = 1/2))



# ----------
# Goodness of Fit test
# summary(data_fit1) does not have X^2/df statistic, so need to calculate by yourself
data_df1 <- as.data.frame(data)
k <- 0:10

Exp1 <- data_fit1$fitted
Diff1 <- data_fit1$observed - Exp1
Chisq1 <- Diff1^2 / Exp1

data_df1 <- data.frame(data_df1, Exp1, Diff1, Chisq1)
data_df1


# chisq --> 32.56737
sum(Chisq1)


# X^2/df:  df is nrow(data) - 1  --> 3.256737
sum(Chisq1) / 10


# The binomial model Bin(n=10, p=0.5) fits worse than the model with paratmer estimated by ML
summary(data_fit)
pchisq(sum(Chisq1), df = nrow(data) - 1, lower.tail = FALSE)

