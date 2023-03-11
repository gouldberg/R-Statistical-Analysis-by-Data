setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\dry2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dry2
# ------------------------------------------------------------------------------


u <- read.csv("dry2_u.txt", sep = "", header = F, colClasses = "numeric")

y <- read.csv("dry2_y.txt", sep = "", header = F, colClasses = "numeric")


dry2 <- cbind(u, y)


colnames(dry2) <- c("input", "output")


head(dry2)



# ----------
str(dry2)



# ----------
# detrending
output2 <- resid(lm(output ~ time(output), data = dry2))


# demean
dry2$input2 <- dry2$input - mean(dry2$input)


input2 <- dry2$input2




# ------------------------------------------------------------------------------
# Prewhitening by AR(2)
# ------------------------------------------------------------------------------


fit <- arima(output2, order = c(2, 0, 0))


summary(fit)


( ar_coef <- as.numeric(coef(fit)) )


output_pw <- resid(fit)


plot(output_pw, type = "l")



# ----------
# prewhitening
# sides = 1:  filter coeffs are for past values only
input.fil <- stats::filter(dry2$input, filter = c(1, -ar_coef[1], -ar_coef[2]), sides = 1)



# ----------
# Check cross - correlation

ccf(output_pw, input.fil, na.action = na.omit, ylab = "CCF", panel.first = grid)




# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------


dry2_t <- ts.intersect(output = as.ts(output_pw),
                       i2 = stats::lag(input.fil, -2),
                       i4 = stats::lag(input.fil, -4),
                       i5 = stats::lag(input.fil, -5))



( u.pw <- lm(output ~ i2 + i4 + i5, data = dry2_t, na.action = na.exclude) )


summary(u.pw)



# ----------
coef(u.pw)




# ------------------------------------------------------------------------------
# Assess residuals
# ------------------------------------------------------------------------------

acf2(resid(u.pw))




# ------------------------------------------------------------------------------
# ARIMAX model
# ------------------------------------------------------------------------------


arx <- sarima(dry2_t[-c(1,2),1], 5, 0, 5, xreg = dry2_t[-c(1,2),c(2,3,4)])


arx



# -->
# not significant terms ...

# but have to recover back to original time series ..

