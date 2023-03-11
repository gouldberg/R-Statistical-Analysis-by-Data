# ------------------------------------------------------------------------------
# Predict the number of second round votes for Mitterand by first round votes
# 
# Weighted Least Squares:  the error will have a variance in proportion to the number of votes (from department to department)
# NOTE: EI is varying from department to department
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/")

packages <- c("faraway", "dplyr", "Hmisc", "lattice", "ggplot2", "corrplot")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------
data(fpe, package="faraway")
str(fpe)
dim(fpe)


# ------------------------------------------------------------------------------
# Basic analysis
# 
# numbers in thousands
# A and B for Mitterand's and Giscard's votes in the first round
# A2 and B2 represent their votes in the second round
# C-K are the first round votes of the other candidates
# E1: registered voters
# N: the difference between first round votes and second round votes = sum(A2+B2) - sum(A-K)
# ------------------------------------------------------------------------------
fpe %>% head()

describe(fpe)

fpe %>% transform(sum1 = A + B + C + D + E + F + G + H + J + K, sum2 = A2 + B2) %>% transform(dif = sum2 - sum1) %>% head()

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", "#007FFF", "blue","#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue"))
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))
wb <- c("white", "black")

corrplot(cor(fpe), method = "color", col = col1(20), cl.length = 21, order = "AOE", addCoef.col = "grey")
corrplot(cor(fpe), order = "hclust", hclust.method = "ward.D2", addrect = 4)
cor.mtest(fpe, conf.level = 0.95)

# EI has large variance
histogram(fpe$EI, breaks = seq(0,1100,by=5))


# ------------------------------------------------------------------------------
# Predict second round votes for Mitterand by first round votes
#
# Normal Regression
# No intercept
# Weighted Least Squares
# Penalized Constrained Least Squares with Weighted Least Squares
# ------------------------------------------------------------------------------
# normal regression
lmod <- lm(A2 ~ A + B + C + D + E + F + G  + H + J + K + N, data = fpe)
summary(lmod)

step(lmod, direction="both", k = log(nrow(fpe)))

lmod_result <- lm(A2 ~ A + B + C + D + E + F + G  + H + K + N, data = fpe)
summary(lmod_result)
plot(residuals(lmod_result) ~ A2, data = fpe)
plot(lmod_result)


# no intercept  --> BETTER
lmod <- lm(A2 ~ A + B + C + D + E + F + G  + H + J + K + N - 1, data = fpe)
summary(lmod)

step(lmod, direction="both", k = log(nrow(fpe)))

lmod_result <- lm(A2 ~ A + B + C + D + E + F + G  + H + N - 1, data = fpe)
summary(lmod_result)
plot(residuals(lmod_result) ~ A2, data = fpe)
plot(lmod_result)

coef(lmod_result)


# Weighted Least Squares
lmod <- lm(A2 ~ A + B + C + D + E + F + G  + H + J + K + N - 1, data = fpe, weights = 1/EI)
summary(lmod)

step(lmod, direction="both", k = log(nrow(fpe)))

lmod_result_wls <- lm(A2 ~ A + B + C + D + E + F + G  + K + N - 1, data = fpe)
summary(lmod_result_wls)
plot(residuals(lmod_result_wls) ~ A2, data = fpe)
plot(lmod_result_wls)

coef(lmod_result_wls)
coef(lmod_result)

AIC(lmod_result_wls)
AIC(lmod_result)


# Weighted Least Squares and restrict coefficients to zero to specified variable
lmod <- lm(A2 ~ offset(A + G + K) + B + C + D + E + F + H + J + N - 1, data = fpe, weights = 1/EI)
summary(lmod)

step(lmod, direction="both", k = log(nrow(fpe)))

lmod_result_wls2 <- lm(A2 ~ offset(A + G + K) + C + D + E + F + N - 1, data = fpe)
summary(lmod_result_wls2)
plot(residuals(lmod_result_wls2) ~ A2, data = fpe)
plot(lmod_result_wls2)

coef(lmod_result_wls2)
coef(lmod_result_wls)
coef(lmod_result)


# Weighted Least Squares and restrict coefficients to from 0 to 1  (Penalized Constrained Least Squares Fitting)
library(mgcv)

lmod <- lm(A2 ~ A + B + C + D + E + F + G  + H + J + K + N - 1, data = fpe, weights=1/EI)
M <- list(w=1/fpe$EI, X=model.matrix(lmod), y=fpe$A2, Ain=rbind(diag(11), -diag(11)), C=matrix(0,0,0), array(0,0), S=list(), off=NULL, p=rep(0.5, 11), bin=c(rep(0,11), rep(-1,11)))

a <- pcls(M)
names(a) <- colnames(model.matrix(lmod))
round(a,3)




