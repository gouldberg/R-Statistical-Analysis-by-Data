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
# Lagged regression
# ------------------------------------------------------------------------------


summary(fit_var)




hc <- ts(hc)

vp <- ts(vp)

prs <- ts(prs)


prs_m2 <- ts.intersect(prs = stats::lag(prs, 0), 
                       prs1 = stats::lag(prs, -1), hc1 = stats::lag(hc, -1), vp1 = stats::lag(vp, -1),
                       prs2 = stats::lag(prs, -2), hc2 = stats::lag(hc, -2), vp2 = stats::lag(vp, -2),
                       trend = time(prs), prs10 = stats::lag(prs, -10), dframe = TRUE)


( u <- lm(prs ~ prs1 + prs2 + hc1 + hc2 + vp1 + vp2 + trend, data = prs_m2) )

( u <- lm(prs ~ prs1 + prs2 + hc2 + vp1 + vp2 + trend, data = prs_m2) )

( u <- lm(prs ~ prs1 + prs2 + vp1 + vp2 + trend, data = prs_m2) )

summary(u)




# ----------

acf2(resid(u))




# ----------
# add prs10 to remove partial auto-correlation at lag 10

( u <- lm(prs ~ prs1 + prs2 + hc2 + hc2 + vp1 + vp2 + trend + prs10, data = prs_m2) )

acf2(resid(u))




# ----------
pred_lagr <- predict(u)

par(mfrow = c(1,1))

plot(pred_lagr, type = "l", ylab = "Extrusion Process: Pressure", lwd = 2, col = 4)

points(prs[11:351])



