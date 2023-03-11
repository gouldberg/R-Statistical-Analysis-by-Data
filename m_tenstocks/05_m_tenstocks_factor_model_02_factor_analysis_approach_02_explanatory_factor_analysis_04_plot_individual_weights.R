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
# weights in 2D
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1))


resFA3$weights

tmp <- data.frame(resFA3$weights)

tmp <- tmp %>% mutate(id = rownames(resFA3$weight), sector = c(1,1,1,1,2,2,2,3,3,3))



# ----------
graphics.off()
par(mfrow = c(1,2))

plot(ML2 ~ ML1, data = tmp, cex = 2, pch = tmp$sector, col = tmp$sector, 
     xlim = c(-0.1, 0.8), ylim = c(-0.1, 0.4), main = "weights ML1 and ML2", cex.main = 2)
text(tmp$ML1 + 0.02, tmp$ML2 + 0.02, label = tmp$id, cex = 1.2)
abline(h = 0, v = 0, lty = 2, col = "gray")

plot(ML3 ~ ML1, data = tmp, cex = 2, pch = tmp$sector, col = tmp$sector, 
     xlim = c(-0.1, 0.8), ylim = c(-0.1, 0.4), main = "weights ML1 and ML3", cex.main = 2)
text(tmp$ML1 + 0.02, tmp$ML3 + 0.02, label = tmp$id, cex = 1.2)
abline(h = 0, v = 0, lty = 2, col = "gray")



# -->
# GS is really close to ML1
# INTC is close to ML2
# LLY is close to ML3


