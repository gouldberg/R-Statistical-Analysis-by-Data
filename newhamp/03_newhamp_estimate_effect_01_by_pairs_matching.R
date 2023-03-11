
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
# Matching 'H' and 'D' ward by similar 'Dean' (formerly proportion by Dean)
# ------------------------------------------------------------------------------

library(Matching)


set.seed(123)



# ----------
# caliper = 0.05:  we accept anything within 0.05 standard deviations of Dean.

# ties = FALSE:  no ties will be allowed so that each treatment ward ('H' ward) will be
# matched to just one control ('D' ward)

# The genertic algorithm functions more effectively with a larger 'population size'


mm <- GenMatch(Tr = dat$trt, X = dat$Dean, ties = FALSE, caliper = 0.05, pop.size = 1000)




# ----------
head(mm$matches)


dat[c(4,213),]



# -->
# 1st column:  the row numbers of the treated observations in the matched dataset
# 2nd column:  the row numbers of the control observations
# 3rd column:  the weight that each matched pair is given

# 1st pair uses cases 4 and 213




# ----------
obj <- c(mm$matches[,1], mm$matches[,2])

par(mfrow = c(1,2))

boxplot(dat$Dean[obj] ~ dat$trt[obj])

boxplot(dat$pObama[obj] ~ dat$trt[obj])




# -->
# the distribution of Dean by trt of only matched pairs are similar
# also the distribution of pObama by trt of only matched pairs are NOT similar




# ----------
# shows matched pairs
par(mfrow = c(1,1))

plot(pObama ~ Dean, data = dat, pch = trt + 1)

with(dat, segments(Dean[mm$match[,1]], pObama[mm$match[,1]], Dean[mm$match[,2]], pObama[mm$match[,2]], col = gray(0.75)))


# ----------
# mean pObama for trt == 0 and 1
abline(h = mean(dat$pObama[dat$trt == 0]), lty = 1)
abline(h = mean(dat$pObama[dat$trt == 1]), lty = 2)


# linear regression for all data
abline(lm(pObama ~ Dean, data = dat), lty = 1)

# linear regression including trt adjustment  --> if controlled by "Dean", the almost no trt effect
coef(lm(pObama ~ Dean + trt, data = dat))
abline(0.2211, 0.5229, lty = 2)




# ------------------------------------------------------------------------------
# Estimate average effect of voting system for Obama proportion by taking mean of difference of matched pairs
# ------------------------------------------------------------------------------

# difference of pObama by mached pairs
pdiff <- dat$pObama[mm$matches[,1]] - dat$pObama[mm$matches[,2]]


# estimated average effect = -0.016
mean(pdiff)



# no clear preference for hand or digital voting.
t.test(pdiff)




# -->
# NOT SIGNIFICANTLY DIFFERENT FROM ZERO !!!
# no clear preference for hand or digital voting.




# ----------
# The difference does not appear to depend on the Dean vote.
plot(pdiff ~ dat$Dean[mm$matches[,1]], xlab = "Dean", ylab = "Hand^Digital")
abline(h = 0)





# ------------------------------------------------------------------------------
# Matching 'H' and 'D' ward by other variables --> different result compared to 'Dean'
# ------------------------------------------------------------------------------


summary(lm(trt ~ Dean, data = dat))
summary(lm(pObama ~ Dean, data = dat))


summary(lm(trt ~ povrate, data = dat))
summary(lm(pObama ~ povrate, data = dat))


summary(lm(trt ~ white, data = dat))
summary(lm(pObama ~ white, data = dat))


summary(lm(trt ~ pci, data = dat))
summary(lm(pObama ~ pci, data = dat))


summary(lm(trt ~ absentee, data = dat))
summary(lm(pObama ~ absentee, data = dat))



# -->
# white, pci and absentee are significant to both of trt and pObama




# ----------
# but we use only "white"
mm2 <- GenMatch(Tr = dat$trt, X = dat$white, ties = FALSE, caliper = 0.05, pop.size = 1000)
# mm2 <- GenMatch(Tr = dat$trt, X = dat$absentee, ties = FALSE, caliper = 0.01, pop.size = 1000)
# mm2 <- GenMatch(Tr = dat$trt, X = cbind(dat$white, dat$absentee), ties = FALSE, caliper = 0.01, pop.size = 1000)




# ----------
obj <- c(mm2$matches[,1], mm2$matches[,2])

par(mfrow = c(1,2))

boxplot(dat$white[obj] ~ dat$trt[obj])

boxplot(dat$pObama[obj] ~ dat$trt[obj])



# -->
# the distribution of white by trt of only matched pairs are similar
# but the distribution of pObama by trt of only matched pairs are NOT similar




# ----------
# estimated average effect
pdiff2 <- dat$pObama[mm2$matches[,1]] - dat$pObama[mm2$matches[,2]]


mean(pdiff2)



# t test for mean difference  --> SIGNIFICANTLY DIFFERENT
t.test(pdiff2)


