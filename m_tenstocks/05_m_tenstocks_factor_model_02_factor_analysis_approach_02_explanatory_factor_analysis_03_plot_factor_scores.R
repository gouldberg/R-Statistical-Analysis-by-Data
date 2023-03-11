# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
# ------------------------------------------------------------------------------

mtenstocks <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(mtenstocks)


dim(mtenstocks)


car::some(mtenstocks)



# ----------
# log returns

rtn <- log(mtenstocks[,2:11] + 1)



# ------------------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(rtn)


polcor



# ------------------------------------------------------------------------------
# plot factor scores
# ------------------------------------------------------------------------------


resFA3_scores <- factor.scores(rtn, f = resFA3, method = "Thurstone")$scores

# resFA3_scores <- factor.scores(rtn, f = resFA3, method = "Harman")$scores


resFA3$loadings



# ----------
res_scores <- resFA3_scores



# ----------
graphics.off()

par(mfrow = c(3,1), mar = c(2,2,2,2))


plot(res_scores[,"ML1"], type = "l", lty = 1, lwd = 2, col = "black", ylim = c(-4, 4), main = "ML1: main for Investment Bank", cex.main = 2)
abline(v = seq(0, 130, by = 10), lty = 2, col = gray(0.7))
abline(h = c(-2, 2), lty = 2, col = gray(0.7))

plot(res_scores[,"ML2"], type = "l", lty = 1, lwd = 2, col = gray(0.7), ylim = c(-4, 4), main = "ML2: main for Semi-Conductor", cex.main = 2)
abline(v = seq(0, 130, by = 10), lty = 2, col = gray(0.7))
abline(h = c(-2, 2), lty = 2, col = gray(0.7))

plot(res_scores[,"ML3"], type = "l", lty = 1, lwd = 2, col = "blue", ylim = c(-4, 4), main = "ML3: main for Pharmaceutical", cex.main = 2)
abline(v = seq(0, 130, by = 10), lty = 2, col = gray(0.7))
abline(h = c(-2, 2), lty = 2, col = gray(0.7))





# ----------
# for comparison

MTSplot(m1$scores[,1:3])




# ------------------------------------------------------------------------------
# plot factor scores in 2D by year and month
# ------------------------------------------------------------------------------


yyyy <- mtenstocks %>% data.frame() %>% mutate(yyyy = substring(date, 1, 4)) %>% dplyr::select(yyyy) %>% pull()

mm <- mtenstocks %>% data.frame() %>% mutate(mm = substring(date, 5, 6)) %>% dplyr::select(mm) %>% pull()


resFA3_scores <- data.frame(resFA3_scores) %>% mutate(yyyy = yyyy, mm = mm)



# ----------
par(mfrow = c(1,1), mar = c(2,2,2,2))


lattice::xyplot(ML2 ~ ML1 | yyyy, data = resFA3_scores, pch = 20, col = "blue")

# lattice::xyplot(ML2 ~ ML1 | mm, data = resFA3_scores, pch = 20, col = "blue")


lattice::xyplot(ML3 ~ ML2 | yyyy, data = resFA3_scores, pch = 20, col = "blue")

# lattice::xyplot(ML3 ~ ML2 | mm, data = resFA3_scores, pch = 20, col = "blue")


