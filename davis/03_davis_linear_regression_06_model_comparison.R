rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\regression_basics\\davis")



# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------

data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)


data2 <- na.exclude(data)



# ----------
linmod <- lm(weight ~ repwt, data = data2)
linmod2 <- update(linmod, subset = -12)
linmod3 <- lm(weight ~ repwt + sex, data = data2, subset = -12)




# ------------------------------------------------------------------------------
# model comparison: terms and coefficient
# ------------------------------------------------------------------------------

stargazer::stargazer(linmod, linmod2, linmod3, type = "text")




# ------------------------------------------------------------------------------
# model comparison by likelihood-ratio tests and the analysis of variance:  ONLY APPLICABLE TO NETSTED MODELS
# ------------------------------------------------------------------------------


# intercept only model
linmod0 <- update(linmod, . ~ 1)

summary(linmod0)



# ----------
# linmod is better than intercept only model ?  --> better

anova(linmod0, linmod)



# ----------
# linmod3 is better than linmod2 ?  --> NOT BETTER ...

anova(linmod2, linmod3)






# ------------------------------------------------------------------------------
# model comparison by AIC:  APPLICABLE TO NON-NESTED MODELS
# ------------------------------------------------------------------------------

AIC(linmod, linmod2, linmod3)


