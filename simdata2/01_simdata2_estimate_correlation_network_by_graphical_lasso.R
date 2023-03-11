rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\simdata2")




# ------------------------------------------------------------------------------
# data:  generation
# ------------------------------------------------------------------------------

set.seed(12345)

n <- 100


x1 <- rnorm(n = n, mean = 0, sd = 1)


x2 <- rnorm(n = n, mean = 2, sd = 1)


x3 <- rnorm(n = n, mean = -1, sd = 2)


x4 <- rnorm(n = n, mean = 1, sd = 1)



# ----------
graphics.off

par(mfrow = c(2,2))

plot(x1, type = "l")

plot(x2, type = "l")

plot(x3, type = "l")

plot(x4, type = "l")





# ----------
ts1 <- c(1.1 * x4 + 1.2 * x1, 
         0.9 * x1 + 1.1 * x2, 
         0.8 * x2 + 0.8 * x3, 
         1.1 * x3 + 1.2 * x4)

ts2 <- c(1.1 * x1 + 1.2 * x2, 
         0.9 * x2 + 1.1 * x3, 
         0.8 * x3 + 0.8 * x4, 
         1.1 * x4 + 1.2 * x1)

ts3 <- c(1.1 * x2 + 1.2 * x3, 
         0.9 * x3 + 1.1 * x4, 
         0.8 * x4 - 0.8 * x1, 
         1.1 * x1 - 1.2 * x2)

ts4 <- c(1.1 * x3 + 1.2 * x4, 
         0.9 * x4 + 1.1 * x1, 
         -0.8 * x1 - 0.8 * x2, 
         -1.1 * x2 - 1.2 * x3)


tsall <- data.frame(cbind(ts1, ts2, ts3, ts4, class = c(rep(1, 100), rep(2, 100), rep(3, 100), rep(4, 100))))


graphics.off

par(mfrow = c(1,1))

MTSplot(tsall[,1:4])




# ------------------------------------------------------------------------------
# Time-varying correlation network  or  graphical lasso
# ------------------------------------------------------------------------------

library(qgraph)


graphics.off()


span <- 100



###########################################################
# 1st 100 points (1 to 100)

# only 1st 100 point
scatterplotMatrix(formula, data = tsall %>% filter(class == 1),
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c(1,2,3,4), pch = 20)


# -->
# strongly correlated:  (ts1,ts2), (ts3,ts4)
# slightly correlated:  (ts2,ts3)



i <- 100

start_idx <- i - span + 1

end_idx <- start_idx - 1 + span

tmp <- tsall[start_idx : end_idx,1:4]

cormat <- cor(tmp)


gr1 <- qgraph(cormat, layout = "spring", sampleSize = nrow(tmp), graph = "glasso",
       color = c("white", "gray", "orange", "blue"), labels = colnames(tsall[,1:4]))


# gr1 <- qgraph(cormat, layout = "spring", minimum = 0.2, graph = "cor",
#              color = c("white", "gray", "orange", "blue"), labels = colnames(tsall[,1:4]))


print(gr1)



# ts2 is smallest closeness
centralityPlot(gr1, include = "Closeness")





###########################################################
# 2nd 100 points (101 to 200)

# only 2nd 100 point
scatterplotMatrix(formula, data = tsall %>% filter(class == 2),
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c(1,2,3,4), pch = 20)



# -->
# strongly correlated:  (ts2,ts3)
# slightly correlated:  (ts1,ts2), (ts3,ts4)
# negatively correlated:  (ts2, ts4)


i <- 200

( start_idx <- i - span + 1 )

( end_idx <- start_idx - 1 + span )

tmp <- tsall[start_idx:end_idx, 1:4]

cormat <- cor(tmp)


gr2 <- qgraph(cormat, layout = "spring", sampleSize = nrow(tmp), graph = "glasso",
        color = c("white", "gray", "orange", "blue"), labels = colnames(tsall[,1:4]))


# gr2 <- qgraph(cormat, layout = "spring", minimum = 0.2, graph = "cor",
#              color = c("white", "gray", "orange", "blue"), labels = colnames(tsall[,1:4]))


print(gr2)


# ts1 is smallest closeness
centralityPlot(gr2, include = "Closeness")




###########################################################
# 3rd 100 points (201 to 300)

# only 3rd 100 point
scatterplotMatrix(formula, data = tsall %>% filter(class == 3),
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c(1,2,3,4), pch = 20)



# -->
# strongly correlated:  (ts1,ts2), (ts3,ts4)
# slightly correlated:  (ts1,ts3), (ts1,ts4)


i <- 300

( start_idx <- i - span + 1 )

( end_idx <- start_idx - 1 + span )

tmp <- tsall[start_idx:end_idx, 1:4]

cormat <- cor(tmp)


gr3 <- qgraph(cormat, layout = "spring", sampleSize = nrow(tmp), graph = "glasso",
             color = c("white", "gray", "orange", "blue"), labels = colnames(tsall[,1:4]))


print(gr3)


# ts4 is smallest closeness
centralityPlot(gr3, include = "Closeness")




###########################################################
# 4th 100 points (301 to 400)

# only 4th 100 point
scatterplotMatrix(formula, data = tsall %>% filter(class == 4),
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c(1,2,3,4), pch = 20)



# -->
# strongly correlated:
# slightly correlated:  (ts2,ts3), (ts3,ts4)
# negative correlation:  (ts1,ts4)



i <- 400

( start_idx <- i - span + 1 )

( end_idx <- start_idx - 1 + span )

tmp <- tsall[start_idx:end_idx, 1:4]

cormat <- cor(tmp)


gr4 <- qgraph(cormat, layout = "spring", sampleSize = nrow(tmp), graph = "glasso",
              color = c("white", "gray", "orange", "blue"), labels = colnames(tsall[,1:4]))


print(gr4)


# ts2 is smallest closeness
centralityPlot(gr4, include = "Closeness")




###########################################################
# at time point 320, the negative correlation between ts1 and ts4 is detected


i <- 320

( start_idx <- i - span + 1 )

( end_idx <- start_idx - 1 + span )

tmp <- tsall[start_idx:end_idx, 1:4]

cormat <- cor(tmp)


gr5 <- qgraph(cormat, layout = "spring", sampleSize = nrow(tmp), graph = "glasso",
              color = c("white", "gray", "orange", "blue"), labels = colnames(tsall[,1:4]))


print(gr5)



# ----------
par(mfrow = c(2,1))

plot(tsall[,1], type = "l")
abline(v = i, lty = 2)

plot(tsall[,4], type = "l")
abline(v = i, lty = 2)
