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
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

# This yields Lh = 1 / weights^2 = 1 / { h(-m)^2 + h(-m+1)^2 ...+ h(m-1)^2 + h(m)^2 } = 9.232, close to the value of L = 9
# The bandwidth = 9.232 / 480 = 0.019
# modified degrees of freedom is 2 * Lh * 453 / 480 = 17.43

# obtain coefficients of modefied Daniell kernel

( ker <- kernel("modified.daniell", c(3,3)) )

plot(ker)



# ----------
# Lh = 1 / sum(hk^2)  k = -6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6  --> 9.24, which is close to the value of L = 9
1 / sum(ker$coef[c(6,5,4,3,2,1,2,3,4,5,6)]^2)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data

graphics.off()

par(mfrow=c(2,3))

for(i in 1:5){ astsa::mvspec(drate[,i], kernel = ker, taper = 0.1, log = "no", main = paste0(colnames(drate)[i])) }


