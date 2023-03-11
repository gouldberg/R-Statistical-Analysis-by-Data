setwd("//media//kswada//MyFiles//R//eustockmarkets")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EuStockMarkets
# ------------------------------------------------------------------------------
data("EuStockMarkets", package = "datasets")


str(EuStockMarkets)

head(EuStockMarkets)



# ----------
dax <- EuStockMarkets[, "DAX"]

Rdax <- diff(log(dax))



# ------------------------------------------------------------------------------
# Split into train and test data
# ------------------------------------------------------------------------------

set.seed(3210)

rand <- sample(2, length(Rdax), replace = TRUE, prob = c(0.6, 0.4))

table(rand) / length(Rdax)



# ----------
traindata <- subset(Rdax, rand == 1)

testdata <- subset(Rdax, rand == 2)



# ------------------------------------------------------------------------------
# Check train and test data 
# ------------------------------------------------------------------------------

par(mfrow=c(2,1))

MASS::truehist(traindata, xlim = c(-0.1, 0.05))

MASS::truehist(testdata, xlim = c(-0.1, 0.05))




# ------------------------------------------------------------------------------
# Fit best distribution by fitDist()
# ------------------------------------------------------------------------------

m1 <- fitDist(traindata, trace = TRUE)


m1$fits




# ------------------------------------------------------------------------------
# Fit best distribution by fitDistPred() using test data set to find the best-fitting distribution using the validation/test deviance
# ------------------------------------------------------------------------------

m2 <- fitDistPred(traindata, newdata = testdata, trace = TRUE)


m2$fits




# ------------------------------------------------------------------------------
# Changing the seed  --> the results can be different
# ------------------------------------------------------------------------------

set.seed(1230)

rand <- sample(2, length(Rdax), replace = TRUE, prob = c(0.6, 0.4))

table(rand) / length(Rdax)


traindata <- subset(Rdax, rand == 1)

testdata <- subset(Rdax, rand == 2)


m1.1 <- fitDist(traindata, trace = TRUE)

m2.1 <- fitDistPred(traindata, newdata = testdata, trace = TRUE)


m1.1$fits

m2.1$fits




# ------------------------------------------------------------------------------
# Alternative way by chooseDist() and chooseDistPred()
# ------------------------------------------------------------------------------

set.seed(1230)

rand <- sample(2, length(Rdax), replace = TRUE, prob = c(0.6, 0.4))

table(rand) / length(Rdax)


traindata <- data.frame(y = subset(Rdax, rand == 1))

testdata <- data.frame(y = subset(Rdax, rand == 2))



# ----------
m0 <- gamlss(y ~ 1, data = traindata)

t1 <- chooseDist(m0, type = "realline", trace = TRUE)

getOrder(t1, 1)




# ----------
t2 <- chooseDistPred(m0, type = "realline", newdata = testdata, trace = TRUE)


t2
