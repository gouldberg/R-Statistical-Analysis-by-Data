
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\newhamp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  newhamp
# ------------------------------------------------------------------------------


dat <- read.csv("newhamp.txt", header = TRUE, sep = "\t")


str(dat)


car::some(dat)




# ----------
dat$trt <- ifelse(dat$votesys == "H", 1, 0)




# ------------------------------------------------------------------------------
# Effect of voting system ('H': by hand)
# ------------------------------------------------------------------------------


# Strictly, this is binomial response, but the normal is a good approximation for the binomial given a large enough sample
# and probabilities not close to zero or one.


lmod <- lm(pObama ~ trt, data = dat)


summary(lmod)



# -->
# It seems that when hand voting is used, the proportion of voting for Obama is 4% higher.
# We are quite sure that Obama received a significantly higher proportion of the vote in the hand voting wards.




# ------------------------------------------------------------------------------
# How about including "Dean" ('Confounding variable')
# ------------------------------------------------------------------------------


# also for Dean, Dean received 9% higher proportion in 'H' ward compared to 'D' ward

summary(lm(Dean ~ trt, data = dat))



# What happens when including trt and Dean ?
lmodz <- lm(pObama ~ trt + Dean, data = dat)


summary(lmodz)



# -->
# Voting system is no longer statistically significant with a p-value of 0.54.
# Dean shows a positive relationship to the proportion voting for Obama.






