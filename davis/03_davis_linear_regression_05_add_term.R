rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\regression_basics")



# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------

data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)


data2 <- na.exclude(data)



# ----------
linmod <- lm(weight ~ repwt, data = data2)





# ------------------------------------------------------------------------------
# adding term "sex"
# ------------------------------------------------------------------------------


linmod3 <- lm(weight ~ repwt + sex, data = data2, subset = -12)

# linmod3 <- update(linmod, . ~ . + sex, subset = -12)


summary(linmod3)




# ----------
# note that confidence interval for sex coefficient includes zero value

car::Confint(linmod3)
