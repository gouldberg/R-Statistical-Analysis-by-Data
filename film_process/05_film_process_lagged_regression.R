setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\film_process")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Film Extrusion Process
# ------------------------------------------------------------------------------


film <- read.table("FilmProcess.txt", sep = "", header = F, colClasses = "numeric")


head(film)



# ----------
hc <- film$V1

vp <- film$V2

prs <- film$V3

wid <- film$V4




# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
# ------------------------------------------------------------------------------


# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


mod_itoo <- astsa::LagReg(input = hc, output = prs, L = 15, M = 40, threshold = 0.02)





# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function for inverse relationship
# ------------------------------------------------------------------------------

mod_otoi <- astsa::LagReg(input = prs, output = hc, L = 15, M = 40, inverse = TRUE,  threshold = 0.02)




# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------

input <- ts(hc)

output <- ts(prs)


prs_m <- ts.intersect(prs = output, 
                    hc1 = stats::lag(hc, 1),
                    hc6 = stats::lag(hc, 6),
                    hc7 = stats::lag(hc, 7),
                    hc8 = stats::lag(hc, 8), dframe = TRUE)



( u <- lm(prs ~ hc1 + hc6 + hc7 + hc8, data = prs_m) )

( u <- lm(prs ~ hc1, data = prs_m) )


summary(u)



# -->
# it seems not good fit



