setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\q_ungdp_4812")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  q_ungdp_4812
#   - Series of U.S. quarterly gross domestic product (GDP) from the 1st quarter of 1948 to the 2nd quarter of 2012.
#     The original GDP data were obtained from the Federal Reserve Bank at St. Louis.
#     The data were in billions of chained 2005 dollars measured to 1 decimal and seasonally adjusted
#   - Here the data is already logged series.
# ------------------------------------------------------------------------------

data <- read.table("q-ungdp-4812.txt", header = TRUE, sep = "")


str(data)



car::some(data)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

