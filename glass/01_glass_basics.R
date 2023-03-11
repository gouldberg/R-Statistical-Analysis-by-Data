setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\glass")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  glass
#   - Output:  Thickness
#   - Input:   Speed
# ------------------------------------------------------------------------------


glass_y <- read.table("glass_y.txt", sep = "", header = F, colClasses = "numeric")

glass_u <- read.table("glass_u.txt", sep = "", header = F, colClasses = "numeric")


glass <- cbind(glass_u, glass_y)


colnames(glass) <- c("thickn", "speed")



head(glass)





# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

