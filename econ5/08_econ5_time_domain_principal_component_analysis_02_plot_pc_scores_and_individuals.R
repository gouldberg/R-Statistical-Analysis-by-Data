setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")


str(econ5)


car::some(econ5)




# ------------------------------------------------------------------------------
# Data transformation
# ------------------------------------------------------------------------------

# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))


x <- cbind(G, U, C)


MTSplot(x)




# ----------
Ud <- diff(econ5$unemp)

Gd <- diff(econ5$govinv)

Cd <- diff(econ5$consum)



dat <- cbind(Gd, Cd, Ud)



# scaled growth rate
gr_s <- scale(apply(log(econ5), 2, FUN = diff))




# ------------------------------------------------------------------------------
# plot PC scores + individual time series
# ------------------------------------------------------------------------------


# 5 * 5(Comp)
m1$loadings



# 160 * 5(Comp)
m1$scores



# ----------
# individuals scores:  1 - 2 components


( pred <- data.frame(m1$scores[,1:2] %*% t(m1$loadings[,1:2])) )

colnames(pred) <- colnames(gr_s)


res_scores <- data.frame(m1$scores)





# ----------
graphics.off()

par(mfrow = c(3,1), mar = c(2,2,2,2))


# factor scores
plot(res_scores[,"Comp.1"], type = "l", lty = 1, col = "black", ylim = c(-5, 5))
lines(res_scores[,"Comp.2"], type = "l", lty = 2, col = gray(0.3))
lines(res_scores[,"Comp.3"], type = "l", lty = 3, col = gray(0.7))
abline(v = seq(0, 160, by = 10), lty = 2, col = "gray")



# ----------
dimnames(gr_s)[[2]] <- colnames(gr_s)


obj <- "unemp"
obj2 <- "govinv"


# blue: model   black: original time series

plot(pred[,obj], type = "l", lty = 1, lwd = 2, col = "blue", ylim = c(-4, 4))
lines(gr_s[,obj], type = "l", lty = 2, lwd = 1, col = "black")
abline(v = seq(0, 160, by = 10), lty = 2, col = "gray")

plot(pred[,obj2], typ = "l", lty = 1, lwd = 2, col = "blue", ylim = c(-4, 4))
lines(gr_s[,obj2], type = "l", lty = 2, lwd = 1, col = "black")
abline(v = seq(0, 160, by = 10), lty = 2, col = "gray")



# -->
# govinv is almost 100% explained by 1-2 PCs
