setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\gala")



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ------------------------------------------------------------------------------
# data summary
# ------------------------------------------------------------------------------

summary(gala)


psych::describe(gala)




# ------------------------------------------------------------------------------
# Ordinary Linear Regression
# ------------------------------------------------------------------------------

# Not include Endemics
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)


summary(lmod)



# -->
# DF:  30 - (5 + 1) = 24



# ----------
names(lmod)


names(summary(lmod))



# ----------
lmodsum <- summary(lmod)




# ------------------------------------------------------------------------------
# Ordinary Linear Regression:  by manual calculation
# ------------------------------------------------------------------------------


x <- model.matrix(~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)


head(x)



# -->
# note that intercept column is created



# ----------
y <- gala$Species




# ----------
# construct inverse of (x'x)

( xtxi <- solve(t(x) %*% x) )



# ----------
# regression coefficients:  (x'x)^(-1) %*% x'y

( xtxi %*% t(x) %*% y )




# ----------
# better way for the cases where the predictors are strongly correlated (such as big data)
# x'x %*% beta = x'y

solve(crossprod(x, x), crossprod(x, y))




# ------------------------------------------------------------------------------
# Ordinary Linear Regression:  by manual calculation
# estimated sigma
# ------------------------------------------------------------------------------

sqrt( sum(lmod$residuals^2) / lmod$df )

lmodsum$sigma




# ------------------------------------------------------------------------------
# Ordinary Linear Regression:  by manual calculation
# standard errors for coefficients
# ------------------------------------------------------------------------------

# inverse of x'x

xtxi

lmodsum$cov.unscaled



# ----------
# Note "Std.Error"
summary(lmod)

lmodsum$coef

lmodsum$coef[,"Std. Error"]




# ----------
sqrt(diag(xtxi)) * lmodsum$sigma


# S2: expected value of residual sum of squares
S2 <- sum(lmod$residuals^2) / (nrow(gala) - length(coef(lmod)))

sqrt(diag(S2 * xtxi))




# ------------------------------------------------------------------------------
# Ordinary Linear Regression:  by manual calculation
# t stats
# ------------------------------------------------------------------------------


# Note for "t value"
summary(lmod)


# t stats
( tstat <- ( coef(lmod) - 0 ) / lmodsum$coef[,"Std. Error"] )



# ----------
# corresponding p- value for df = 24, t distirbution, both sides (= "2 * ")

df <- 24

2 * pt(tstat, df)


