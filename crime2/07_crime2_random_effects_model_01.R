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
# Random Effects Model
#   - Fixed effects model is appropriate when the unobserved heterogeneity term is correlated with the independent variables.
#   - When this is not the case, we can use either OLS, or, when OLS is not enough, the random effects model
# ------------------------------------------------------------------------------

# We use paneled crime4.p data

# "swar" is default
reg_rnd <- plm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p, model = "random", random.method = "swar")


summary(reg_rnd)




# ----------
# fixed effects model
reg_within <- plm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p, model = "within")


# compare
stargazer::stargazer(reg_within, reg_rnd,
                     header = FALSE, title = "Fixed and Random Effects",
                     type = "text", digits = 4,
                     intercept.bottom = FALSE, column.labels = c("Fixed Effects", "Random Effects"))



# ------------------------------------------------------------------------------
# Compare FGLS to the cluster robust standard errors
# ------------------------------------------------------------------------------

Coefficients <- coef(reg_rnd)


FGLS <- sqrt(diag(vcov(reg_rnd)))


ClusterRobust <- sqrt(diag(vcovHC(reg_rnd, type = "HC0", cluster = "group")))



# ----------
tab <- data.frame(Coefficients, FGLS, ClusterRobust)

kable(tab, digits = 4, align = "c", caption = "Random Effects Estimates with Alternative Errors")




