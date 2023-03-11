setwd("//media//kswada//MyFiles//R//dbbmi")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dbbmi
# ------------------------------------------------------------------------------

dbbmi <- read.csv("dbbmi.txt", header = T, sep = "\t")

# data("dbbmi", package = "gamlss.data")


str(dbbmi)

car::some(dbbmi)




# ------------------------------------------------------------------------------
# Quantile Sheets Regression
#   - simultaneous estimation of the centiles,
#     and the introduction of a smoothing parameter in the response variable direction
#     This reduces the wide variability (i.e., erratic behaviour) of the quantile curves at the extremes
#     and makes them look more realistic
#     It also usually avoids the problem of crossing quantiles
# ------------------------------------------------------------------------------


library(gamlss)


# find a suitable power transformation for age
ppp <- findPower(dbbmi$bmi, dbbmi$age)




# ----------
# model by qunatile sheet regression
# smoothing parameters "p.lambda" is 1 (by default)

qs1 <- quantSheets(bmi, age, data = dbbmi, cent = c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6),
                   x.lambda = 0.1, logit = TRUE, power = ppp)




# ----------
# residual diagnostics

res1 <- resid(qs1)


round(Q.stats(resid = res1, xvar = dbbmi$age), 3)


wp(resid = res1, xvar = dbbmi$age, n.inter = 9)


# -->
# show evidence that the skewness of the response distribution is not modelled properly.




# ----------
# reduce p.lambda to 0.05

qs2 <- quantSheets(bmi, age, data = dbbmi, cent = c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6),
                   x.lambda = 0.1, p.lambda = 0.05, logit = TRUE, power = ppp)


res2 <- resid(qs2)

round(Q.stats(resid = res2, xvar = dbbmi$age), 3)

wp(resid = res2, xvar = dbbmi$age, n.inter = 9)



# -->
# better distribution




# ------------------------------------------------------------------------------
# Quantile Sheets Regression
#   - extract outliers
# ------------------------------------------------------------------------------


newage <- dbbmi$age


head(fitted(qs2))


mat2 <- predict(qs2, newdata = newage)


head(mat2)

tail(mat2)




# ----------
# flag for outlier  --> 66 points

mat2 <- data.frame(cbind(mat2, bmi = dbbmi$bmi))

mat2 <- mat2 %>% mutate(outlier = ifelse(bmi < X0.4, 1, ifelse(bmi > X99.6, 1, 0)))

mat2 %>% filter(outlier == 1)







