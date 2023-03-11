# setwd("//media//kswada//MyFiles//R//melanoma")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\melanoma")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  melanoma
# ------------------------------------------------------------------------------

melanoma <- t(matrix(scan("melanoma.txt", 0), 3, 37)) %>% as.data.frame() %>% dplyr::select(V2, V3)

# data("melanoma", package = "fda")


colnames(melanoma) <- c("year", "incidence")



str(melanoma)




# ------------------------------------------------------------------------------
# data exploration:  plot time series data
# ------------------------------------------------------------------------------

par(mfrow=c(1,1), mar = c(2,2,2,2))
plot(incidence ~ year, data = melanoma, type = "o")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")


# -->
# This is very small data ... only 37 data points

# It seems that there are large cycles (by 10 years) and small sub-cycles
# But it is somwehat difficult to model large cycle and small sub cycles

