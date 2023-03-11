setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\m_bnd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m_bnd
#   - The monthly yields of Moody's seasoned corporate Aaa and Baa bonds from July 1954 to February 2005 for 609 observations.
#     The data were obtained from Federal Reserve Bank of St.Louis.
# ------------------------------------------------------------------------------

data <- read.table("m-bnd.txt", header = FALSE, sep = "")


str(data)



car::some(data)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

