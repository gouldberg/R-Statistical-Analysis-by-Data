# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_unempstatesAdj")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-unempstatesAdj
# ------------------------------------------------------------------------------

da <- read.csv("m-unempstatesAdj.txt", sep = "", header = T)


str(da)


dim(da)


car::some(da)



# ----------
# first difference

drate <- diffM(da)



# ------------------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(drate)


polcor



# ------------------------------------------------------------------------------
# plot factor scores
# ------------------------------------------------------------------------------


resFA3_scores <- factor.scores(da, f = resFA3, method = "Thurstone")$scores
resFA3_scores <- factor.scores(da, f = resFA3, method = "tenBerge")$scores


resFA3$loadings



# ----------
graphics.off()

par(mfrow = c(3,1), mar = c(2,2,2,2))


plot(resFA3_scores[,"ML1"], type = "l", lty = 1, lwd = 2, col = "black", ylim = c(-15, 15), main = "ML1", cex.main = 2)
abline(v = seq(0, 440, by = 40), lty = 2, col = gray(0.7))
abline(h = c(-5, 5), lty = 2, col = gray(0.7))

plot(resFA3_scores[,"ML2"], type = "l", lty = 1, lwd = 2, col = "blue", ylim = c(-15, 15), main = "ML2", cex.main = 2)
abline(v = seq(0, 440, by = 40), lty = 2, col = gray(0.7))
abline(h = c(-5, 5), lty = 2, col = gray(0.7))

plot(resFA3_scores[,"ML3"], type = "l", lty = 1, lwd = 2, col = "darkgray", ylim = c(-15, 15), main = "ML3", cex.main = 2)
abline(v = seq(0, 440, by = 40), lty = 2, col = gray(0.7))
abline(h = c(-5, 5), lty = 2, col = gray(0.7))





# ----------
# for comparison

MTSplot(m1$scores[,1:3])


