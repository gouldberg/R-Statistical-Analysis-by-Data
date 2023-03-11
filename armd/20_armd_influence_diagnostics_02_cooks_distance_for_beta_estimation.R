
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)



# ----------
data("armd", package = "nlmeU")


str(armd)




# ------------------------------------------------------------------------------
# Calculation of Cook's distances for model
#   - Cook's distance measure the scaled change induced by the exclusion of a particular observations
#     in the estimated parameter vector
#   - The larger the Cook's distance, the larger the influence of the i-th observation on the estimate of beta
# ------------------------------------------------------------------------------


# calculation of Cook's distances


betaUall <- sapply(lmeUall, fixef)


vb.inv <- solve(vcovb)

CookDfun <- function(betaU){
  dbetaU <- betaU - beta0
  cookD.value <- t(dbetaU) %*% vb.inv %*% dbetaU
}


CookD.num <- apply(betaUall, 2, CookDfun)


(n.fixeff <- length(beta0))



rankX <- n.fixeff


CookD <- CookD.num / rankX



# ----------
# plot of Cook's distances using traditional graphics.

outD <- CookD > 0.03


subject.c[outD]


plot(CookD ~ subject.c, ylab = "Cook's D", type = "h")

text(as.numeric(subject.c[outD]), CookD[outD], subject.c[outD])

points(subject.c[outD], CookD[outD])



