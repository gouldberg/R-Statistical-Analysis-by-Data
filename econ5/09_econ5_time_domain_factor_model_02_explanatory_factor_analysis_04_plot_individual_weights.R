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
# weights in 2D
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1))


resFA3$weights

tmp <- data.frame(resFA3$weights)

tmp <- tmp %>% mutate(id = rownames(resFA3$weight))



# ----------
graphics.off()
par(mfrow = c(1,2))

plot(ML2 ~ ML1, data = tmp, cex = 2, pch = 1:5, col = 1:5, 
     xlim = c(-1, 1), ylim = c(-1, 1), main = "weights ML1 and ML2", cex.main = 2)
text(tmp$ML1 + 0.02, tmp$ML2 + 0.02, label = tmp$id, cex = 1.2)
abline(h = 0, v = 0, lty = 2, col = "gray")

plot(ML3 ~ ML1, data = tmp, cex = 2, pch = 1:5, col = 1:5, 
     xlim = c(-1, 1), ylim = c(-1, 1), main = "weights ML1 and ML3", cex.main = 2)
text(tmp$ML1 + 0.02, tmp$ML3 + 0.02, label = tmp$id, cex = 1.2)
abline(h = 0, v = 0, lty = 2, col = "gray")



# -->
# gnp is close to ML1 and ML2
# unemp is close to ML3
# prinv is close to ML1

