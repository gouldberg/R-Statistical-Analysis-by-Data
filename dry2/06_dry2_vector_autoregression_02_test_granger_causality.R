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



# ----------
# VAR model

library(vars)


input <- dry2$input2

output <- dry2$output


x <- cbind(input, output)

fit5 <- VAR(x, p = 5, type = "both")

fit6 <- VAR(x, p = 6, type = "both")




# ------------------------------------------------------------------------------
# Granger causality testing by vars::causality()
# ------------------------------------------------------------------------------

# Granger causality H0:  "input" do not Granger-cause "output" --> REJECTED
# H0: No instantaneous causality between input and output --> REJECTED !!!


vars::causality(fit5, cause = "input")




# ----------
# Granger causality H0:  "output" do not Granger-cause "input" --> NOT REJECTED
# H0: No instantaneous causality between output and input --> REJECTED !!!

x2 <- cbind(output, input)

fit5_2 <- VAR(x2, p = 5, type = "both")

summary(fit5_2)


vars::causality(fit5_2, cause = "output")

