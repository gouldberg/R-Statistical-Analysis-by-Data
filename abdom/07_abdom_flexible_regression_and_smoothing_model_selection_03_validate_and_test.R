setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
# ------------------------------------------------------------------------------
data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# Model fitted with gamlssVGD() for validation and test data set
# ------------------------------------------------------------------------------

set.seed(4646)


# ----------
# generate the random split of the data
# 1: training  2: validation data set
rand2 <- sample(2, 610, replace = TRUE, prob = c(0.6, 0.4))


# proportions in the sample
table(rand2) / 610



# ---------
v1 <- gamlssVGD(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = NO, rand = rand2)


v2 <- gamlssVGD(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = LO, rand = rand2)


v3 <- gamlssVGD(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = TF, rand = rand2)



# compare models based on global deviance by VGD()
VGD(v1, v2, v3)



# ------------------------------------------------------------------------------
# Split two data sets in advance
# ------------------------------------------------------------------------------

training <- abdom[rand2 == 1,]

validation <- abdom[rand2 == 2,]



# ----------
v11 <- gamlssVGD(y ~ pb(x), sigma.formula = ~ pb(x), data = training, family = NO, newdata = validation)


v12 <- gamlssVGD(y ~ pb(x), sigma.formula = ~ pb(x), data = training, family = LO, newdata = validation)


v13 <- gamlssVGD(y ~ pb(x), sigma.formula = ~ pb(x), data = training, family = TF, newdata = validation)


# compare models based on global deviance by VGD()
VGD(v11, v12, v13)



# ----------
g1 <- gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = training, family = NO)

g2 <- gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = training, family = LO)

g3 <- gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = training, family = TF)


gg1 <- getTGD(g1, newdata = validation)
gg2 <- getTGD(g2, newdata = validation)
gg3 <- getTGD(g3, newdata = validation)

TGD(gg1, gg2, gg3)



