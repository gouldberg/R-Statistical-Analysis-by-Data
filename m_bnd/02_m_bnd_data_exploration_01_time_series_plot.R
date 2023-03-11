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
# data exploration:  multivariate time series plot
# ------------------------------------------------------------------------------

graphics.off()


MTSplot(data)


