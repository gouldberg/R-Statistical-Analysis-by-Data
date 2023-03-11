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
# Impulse Responses Function:  How much 1 SD fluctuation impacts to output ?
# ------------------------------------------------------------------------------

# default is ortho = TRUE

impresp <- vars::irf(fit5, n.ahead = 25)



# ----------
graphics.off()


plot(impresp, plot.type = "m")


round(impresp$irf$input, 3)

round(impresp$irf$output, 3)




# ------------------------------------------------------------------------------
# Impulse Responses Function by MTS::VARirf()
# ------------------------------------------------------------------------------


MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma, orth=F, lag = 25)


MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma, lag = 25)




# ------------------------------------------------------------------------------
# Forecast Variance Decomposition
#    - estimates the contribution of a shock in each variable to the reponse in both variables.
# ------------------------------------------------------------------------------

fvd <- vars::fevd(fit5, n.ahead = 25)

fvd$output


plot(fvd)



# -->
# output at lag 6 is explained only 57.8% by input




# ------------------------------------------------------------------------------
# Forecast ERROR Variance Decomposition
# ------------------------------------------------------------------------------

# by MTS:  Forecast Error Vairance Decomposition

MTS::FEVdec(Phi = varfit_MTS_ref$Phi, Theta = NULL, Sig = varfit_MTS_ref$Sigma, lag = 25)

