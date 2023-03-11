setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\glass")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  glass
#   - Output:  Thickness
#   - Input:   Speed
# ------------------------------------------------------------------------------


glass_y <- read.table("glass_y.txt", sep = "", header = F, colClasses = "numeric")

glass_u <- read.table("glass_u.txt", sep = "", header = F, colClasses = "numeric")


glass <- cbind(glass_u, glass_y)


colnames(glass) <- c("input", "output")



head(glass)




# ----------
# demean input

glass$input2 <- glass$input - mean(glass$input)




# ------------------------------------------------------------------------------
# Check cross correlation
# ------------------------------------------------------------------------------

input <- glass$input2

output <- glass$output


# check cross correlation first

par(mfrow = c(1,1))


ccf(output, input, lag = 20)




# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------


mod_l <- astsa::LagReg(input = input, output = output, L = 15, M = 40, threshold = 0.02)



glass_t <- ts.intersect(output = stats::lag(output, 0),
                       i2 = stats::lag(input, -2),
                       i5 = stats::lag(input, -5),
                       i10 = stats::lag(input, -10),
                       i11 = stats::lag(input, -11),
                       i12 = stats::lag(input, -12),
                       i13 = stats::lag(input, -13),
                       i14 = stats::lag(input, -14),
                       i19 = stats::lag(input, -19))
                       


( u0 <- lm(output ~ i2 + i5 + i10+ i11 + i12 + i13 + i14 + i19, data = glass_t, na.action = NULL) )


( u1 <- lm(output ~ i10+ i11 + i12 + i13 + i14 + i19, data = glass_t, na.action = NULL) )


summary(u0)

summary(u1)



# ----------
# the model can be simplified (u1)
anova(u0, u1)



AIC(u0, u1)



coef(u2)




# ------------------------------------------------------------------------------
# Assess residuals
# ------------------------------------------------------------------------------

acf2(resid(u1))




# ------------------------------------------------------------------------------
# ARIMAX model
# ------------------------------------------------------------------------------


arx <- sarima(glass_t[,1], 4, 0, 4, xreg = glass_t[,c(4,5,6,7,8,9)])


arx




# ------------------------------------------------------------------------------
# 1-step-ahead predictions
# ------------------------------------------------------------------------------

pred <- output[-c(1:19)] + resid(arx$fit)

par(mfrow = c(1,1))

ts.plot(pred, output[-c(1:19)], col = c('skyblue', "black"), lwd = c(7,2))


