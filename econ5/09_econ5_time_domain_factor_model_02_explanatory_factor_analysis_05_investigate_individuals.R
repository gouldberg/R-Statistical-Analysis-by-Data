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
# Individual scores
# ------------------------------------------------------------------------------

# we apply unorthogonal solutions (resFA3)


# 5 * 3
round(resFA3$weights, 3)


# 5 * 3
resFA3$loadings


# 160 * 3
resFA3_scores <- factor.scores(gr_s, f = resFA3, method = "Thurstone")$scores
# resFA3_scores <- factor.scores(gr_s, f = resFA3, method = "Anderson")$scores


( res_scores <- data.frame(resFA3_scores) )




# ----------
# individuals scores

( pred <- resFA3_scores %*% t(resFA3$weights) )



# unemp
head(rtn[,"unemp"])

head(pred[,"unemp"])


resFA3$weights["unemp",]

resFA3$loadings["unemp",]

head(resFA3_scores)




# ------------------------------------------------------------------------------
# plot: original time series, individual scores, main factor scores
# ------------------------------------------------------------------------------

resFA3wts <- data.frame(resFA3$weights)


# black: model
# blue: original time series
# red: Xth factor loagings

graphics.off()

par(mfrow = c(3,2), mar = c(2,2,2,2))


obj <- c("unemp", "gnp", "consum", "govinv", "prinv")

obj_fa <- "ML1"
# obj_fa <- "ML2"
# obj_fa <- "ML3"


sort(round(resFA3$communality, 4))

for(i in 1:length(obj)){
  
  plot(pred[,obj[i]], type = "l", lty = 1, lwd = 2, col = "black", ylim = c(-2, 2), 
       main = paste0(obj[i], " : ", obj_fa, " : weights ", round(resFA3wts[obj[i], obj_fa], 3)))
  abline(v = seq(0, 130, by = 10), lty = 2, col = gray(0.7))
  lines(gr_s[,obj[i]], type = "l", lty = 1, lwd = 1, col = "blue")
  lines(res_scores[,obj_fa], type = "l", lty = 1, lwd = 1, col = "red", ylim = c(-2, 2))
}


