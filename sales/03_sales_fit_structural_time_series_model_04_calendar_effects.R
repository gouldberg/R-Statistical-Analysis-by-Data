setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\00_basics\\10_time_series_basics\\sales")

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

sun <- weeks[,"��"]

mon <- weeks[,"��"]-sun; tue <- weeks[,"��"]-sun; wed <- weeks[,"��"]-sun

thu <- weeks[,"��"]-sun; fry <- weeks[,"��"]-sun; sat <- weeks[,"�y"]-sun

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
plot(kfsCalender$muhat - kfsCalender$alphahat[,"level"], type = "l", xaxs = "i", xaxt = "n", xlab = "", ylab = "�̔��z�i10���~�j")

axis(side = 1, at = 1+0:11*12,
     labels=c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))




# -->
# seasonality is fixed (sea.type = "dummy"), but by including leapyear and calendar,
# the seasonality is captured very well.


