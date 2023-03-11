setwd("//media//kswada//MyFiles//R//bnrf1ebv")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bnrf1ebv
# ------------------------------------------------------------------------------

data(bnrf1ebv, package = "astsa")

bnrf1ebv



# ------------------------------------------------------------------------------
# data exploration:  Plot sequence in different coding
# ------------------------------------------------------------------------------

# 1=A, 2=C, 3=G, 4=T  --> convert to other coding
from <- c(1, 2, 3, 4)

to <- c(1, 1, 0, 0)
cod2 <- to[match(bnrf1ebv, from)]

to <- c(0, 1, 1, 0)
cod3 <- to[match(bnrf1ebv, from)]

to <- c(0, 0, 1, 1)
cod4 <- to[match(bnrf1ebv, from)]



# ----------
obj <- 1:50

graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,2))
plot(bnrf1ebv[obj], ylab = "", xlab = "", main = "Original", type = "l")
plot(cod2[obj], ylab = "", xlab = "", main = "Coding 2", type = "l")
plot(cod3[obj], ylab = "", xlab = "", main = "Coding 3", type = "l")
plot(cod4[obj], ylab = "", xlab = "", main = "Coding 4", type = "l")



