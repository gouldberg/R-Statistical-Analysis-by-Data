setwd("//media//kswada//MyFiles//R//crime2")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crime2
#  - balanced panel of 46 cities, properly sorted, which is discused by Wooldridge (2016, Secion 13.3)
# ------------------------------------------------------------------------------

# library(foreign)
# crime2 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/crime2.dta")
# write.table(crime2, file = "crime2.txt", row.names = F, quote = F, sep = "\t")

# library(foreign)
# crime4 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/crime4.dta")
# write.table(crime4, file = "crime4.txt", row.names = F, quote = F, sep = "\t")


crime2 <- read.table("crime2.txt", header = T, stringsAsFactors = FALSE, sep = "\t")

crime4 <- read.table("crime4.txt", header = T, stringsAsFactors = FALSE, sep = "\t")


str(crime2)
str(crime4)

dim(crime2)
dim(crime4)

car::some(crime2)
car::some(crime4)



# ------------------------------------------------------------------------------
# Estimate individual intercepts explicitly  (Least Squares Dummy Variable Model)
#   - includes a dummy variable for each individual in the panel
#   - This model is equivalent to the "within" estimator model
#   - The fixed effects model requires testing for unobserved heterogeneity, or individual differneces
#
#   - H0:  all coefficients of the dummy variables are zero
#   - H1:  al least a coefficient of a dummy variable is not zero
# ------------------------------------------------------------------------------


# Least Squares Dummy variable Model
# In order to use pFtest, use plm  (but equivalent with reg_lsdv)

# reg_lsdv <- lm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + county - 1, data = crime4.p)
reg_within <- plm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p, model = "within", effect = "time")



# pooled model
reg_pool <- lm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p)



lmtest::coeftest(reg_lsdv)



# ----------
# F test for time effects
pFtest(reg_lsdv, reg_pool)


# -->
# time effects are not statistically significant ...




# ------------------------------------------------------------------------------
# Test the individual effect
# ------------------------------------------------------------------------------


# Fixed effects model
reg_within2 <- plm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p, model = "within", effect = "individual")


summary(reg_within2)



# ----------
# F test for individual
pFtest(reg_within2, reg_pool)



# -->
# individual effects are statistically significant



# ------------------------------------------------------------------------------
# Fixed effects with cluster robust standard errors
# ------------------------------------------------------------------------------

# We use paneled crime4.p data

reg_within <- plm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p, model = "within")


Conventional <- sqrt(diag(vcov(reg_within)))


ClusterRobust <- sqrt(diag(vcovHC(reg_within, type = "HC0", cluster = "group")))



# ----------
tab <- data.frame(Conventional, ClusterRobust)

kable(tab, digits = 4, align = "c", caption = "Fixed Effects Estimates with Alternative Errors")
