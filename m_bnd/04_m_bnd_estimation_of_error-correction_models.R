setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\m_bnd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m_bnd
# ------------------------------------------------------------------------------

data <- read.table("m-bnd.txt", header = FALSE, sep = "")


str(data)



car::some(data)



# ----------
data <- data[,c(4:5)]


colnames(data) <- c("Aaa", "Baa")




# ------------------------------------------------------------------------------
# Estimation of Error-Correction VAR models
# ------------------------------------------------------------------------------


# cointegrating series

wt <- data[,1] - 0.886 * data[,2]




# ----------
# Co-integrated series is given

m1 <- MTS::ECMvar1(data, 3, wt)




# ----------
# Refine the model

m2 <- MTS::refECMvar1(m1)




# ----------
# Joint estimation

( beta <- c(1, -0.886) )

m3 <- MTS::ECMvar(data, p = 3, ibeta = beta, include.const = F)


