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
# Determining the number of factors:  Tucker-Lewis index (TLI)
#   - it compares a worst-case model Q0 (a zero-factor model) and a best-case model with our fitted model Qp
#     TLI(p) = ( Q0 - Qp ) / ( Q0 - 1 )
#   - the larger its value the better the fit.  Hu and Bentler (1999) consider values above 0.90 as "good" fit
# ------------------------------------------------------------------------------

fa_1 <- fa(rtn, 1, cor = "cor", fm = "ml")

fa_2 <- fa(rtn, 2, cor = "cor", fm = "ml")

fa_3 <- fa(rtn, 3, cor = "cor", fm = "ml")



# ----------
summary(fa_1)


summary(fa_2)


summary(fa_3)



# -->
# TLI(1) = 0.633   TLI(2) = 0.72   TLI(3) = 0.804 < 0.83
# We see that the TLI is smaller than 0.90, and the RMSEA indicates NOT good fit (= 0.132 > 0.05)
