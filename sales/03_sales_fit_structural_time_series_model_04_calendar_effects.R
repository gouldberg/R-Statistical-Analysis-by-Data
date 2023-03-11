setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\10_time_series_basics\\sales")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sales
# ------------------------------------------------------------------------------


dat <- read.csv("sales.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


str(dat)


car::some(dat)



# ----------
colnames(dat) <- c("month", "fabrics", "machinery", "fuel")




# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood)
# calendar effects
# ------------------------------------------------------------------------------


library(KFAS)


# ----------
# weekdays dummy variable

dates <- seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by = 1)

weeks <- table(substr(dates,1,7), weekdays(dates, T))

sun <- weeks[,"日"]

mon <- weeks[,"月"]-sun; tue <- weeks[,"火"]-sun; wed <- weeks[,"水"]-sun

thu <- weeks[,"木"]-sun; fry <- weeks[,"金"]-sun; sat <- weeks[,"土"]-sun

( calendar <- cbind(mon, tue, wed, thu, fry, sat) )




# ----------
# leap year dummy variable

leapyear <- rownames(weeks) %in% c("2004-02","2008-02","2012-02")



# ----------
# calendar effects model (weekdays and leap year)
# add dummy variable: leapyear and calendar

modCalender <- SSModel(dat$fabrics ~ SSMtrend(2, Q = c(list(0), list(NA)))
                       + SSMseasonal(12, sea.type="dummy")
                       + leapyear + calendar, H = NA)

fitCalender <- fitSSM(modCalender, numeric(2), method = "BFGS")

kfsCalender <- KFS(fitCalender$model)



# ----------
graphics.off()

par(mfrow = c(1,1))


# seasonality + calendar effects
plot(kfsCalender$muhat - kfsCalender$alphahat[,"level"], type = "l", xaxs = "i", xaxt = "n", xlab = "", ylab = "販売額（10億円）")

axis(side = 1, at = 1+0:11*12,
     labels=c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))




# -->
# seasonality is fixed (sea.type = "dummy"), but by including leapyear and calendar,
# the seasonality is captured very well.


