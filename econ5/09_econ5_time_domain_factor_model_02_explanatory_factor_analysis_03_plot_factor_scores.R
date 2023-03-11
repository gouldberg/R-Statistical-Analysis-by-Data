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
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(gr_s)


polcor




# ------------------------------------------------------------------------------
# plot factor scores
# ------------------------------------------------------------------------------


resFA3_scores <- factor.scores(gr_s, f = resFA3, method = "Thurstone")$scores

# resFA3_scores <- factor.scores(gr_s, f = resFA3, method = "Harman")$scores


resFA3$loadings



# ----------
res_scores <- resFA3_scores



# ----------
graphics.off()

par(mfrow = c(3,1), mar = c(2,2,2,2))


plot(res_scores[,"ML1"], type = "l", lty = 1, lwd = 2, col = "black", ylim = c(-4, 4), main = "ML1: main for gnp and prinv", cex.main = 2)
abline(v = seq(0, 160, by = 10), lty = 2, col = gray(0.7))
abline(h = c(-2, 2), lty = 2, col = gray(0.7))

plot(res_scores[,"ML2"], type = "l", lty = 1, lwd = 2, col = gray(0.7), ylim = c(-4, 4), main = "ML2: main for consum", cex.main = 2)
abline(v = seq(0, 160, by = 10), lty = 2, col = gray(0.7))
abline(h = c(-2, 2), lty = 2, col = gray(0.7))

plot(res_scores[,"ML3"], type = "l", lty = 1, lwd = 2, col = "blue", ylim = c(-4, 4), main = "ML3: main for unemp", cex.main = 2)
abline(v = seq(0, 160, by = 10), lty = 2, col = gray(0.7))
abline(h = c(-2, 2), lty = 2, col = gray(0.7))




# ----------
# for comparison (PCA analysis)

MTSplot(m1$scores[,1:3])


