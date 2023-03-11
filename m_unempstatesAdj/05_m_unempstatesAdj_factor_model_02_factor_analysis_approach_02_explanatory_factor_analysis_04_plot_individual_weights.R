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

plot(ML2 ~ ML1, data = tmp, cex = 2, pch = 20, col = "black", 
     xlim = c(-0.1, 0.2), ylim = c(-0.1, 0.2), main = "weights ML1 and ML2", cex.main = 2)
text(tmp$ML1 + 0.02, tmp$ML2 + 0.02, label = tmp$id, cex = 1.2)
abline(h = 0, v = 0, lty = 2, col = "gray")

plot(ML3 ~ ML1, data = tmp, cex = 2, pch = 20, col = "black", 
     xlim = c(-0.1, 0.2), ylim = c(-0.2, 0.2), main = "weights ML1 and ML3", cex.main = 2)
text(tmp$ML1 + 0.02, tmp$ML3 + 0.02, label = tmp$id, cex = 1.2)
abline(h = 0, v = 0, lty = 2, col = "gray")



# -->
# MN1 is largest in ML1 weight but small (only 0.12)
# MA is largest in ML2 (0.181)


