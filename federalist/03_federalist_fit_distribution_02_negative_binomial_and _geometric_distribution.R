setwd("//media//kswada//MyFiles//R//federalist")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Federalist
# ------------------------------------------------------------------------------
data("Federalist", package = "vcd")

data <- Federalist

data
sum(data)



# ------------------------------------------------------------------------------
# Fit the negative binomial distribution
#
#  - The negative binomial distribution is a type of waiting-time distribution, but also arises in statistical applications
#   as a generalization of the Poisson distribution, allowing for overdispersion (variance > mean).
#   One form of the negative binomial distribution (also called the Pascal distribution) arises when a series of independent Bernoulli trials
#   is observed with constant probability p of some event, and we ask how many non-events (failures), k, it takes to observe n successfule events.
# ------------------------------------------------------------------------------
# firstly, poisson distribution for comparison

data_fit0 <- vcd::goodfit(data, type = "poisson")


data_fit1 <- vcd::goodfit(data, type = "nbinomial")



# ----------
# estimated paramters by maximum likelihood estimation

unlist(data_fit1$par)



# ----------
# Better fit than poisson distribution

print(data_fit0, digits=2)

print(data_fit1, digits=2)



# ----------
# The GOF test shows good fit

summary(data_fit0)

summary(data_fit1)




# -->
# One interpretation of the better fit of the negative binomial is that
# the use of a given word occurs with Poisson frequencies,
# but Madison varied its rate lambda, from one block of text to another
# acoording to a gamma distribution,
# allowing the variance to be greater than mean.




# ------------------------------------------------------------------------------
# Also Fit the geometric distribution
#
#  - The geometric distribution is the special case of the negative binomial distribution when n = 1
#    We observe a series of independent trails and count the number of non-events (failures) preceeding the first sucessful event
# ------------------------------------------------------------------------------

tab <- as.data.frame(data, stringsAsFactors = FALSE)


colnames(tab) <- c("nMay", "Freq")


str(tab)



# ----------
# Calculate the probabilithies (phat) of k = 0:6 counts, and hence the expected (exp) frequencies in a Poisson distribution

k <- 0:6

n <- 1

mu <- weighted.mean(as.numeric(tab$nMay), w = tab$Freq)


phat <- dnbinom(k, n, mu = mu)

exp <- sum(tab$Freq) * phat

chisq <- (tab$Freq - exp)^2 / exp


( GOF <- data.frame(tab, phat, exp, chisq) )



# ----------
# Chisq = 1.513097 and shows a good fit
sum(chisq)

pchisq(sum(chisq), df = nrow(tab) - 2, lower.tail  = FALSE)




# ------------------------------------------------------------------------------
# Also Fit the geometric distribution using goodfit()
# ------------------------------------------------------------------------------

data_fit2 <- vcd::goodfit(data, type = "nbinomial", par=list(size = 1))

unlist(data_fit1$par)

unlist(data_fit2$par)

print(data_fit1, digits=2)

print(data_fit2, digits=2)


summary(data_fit1)

summary(data_fit2)

