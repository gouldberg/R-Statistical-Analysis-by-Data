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
# Correlation analysis:  autocorrelation and partial-correaltion
# ------------------------------------------------------------------------------

graphics.off()

astsa::acf2(hc, max.lag = 20, main = "Heater Current")

astsa::sarima(p = 2, d = 0, q = 2, xdata = hc)

astsa::sarima(p = 1, d = 0, q = 2, xdata = hc)





# ----------

astsa::acf2(vp, max.lag = 20, main = "Valve Position")

astsa::sarima(p = 2, d = 0, q = 2, xdata = vp)

astsa::sarima(p = 1, d = 0, q = 2, xdata = vp)

astsa::sarima(p = 0, d = 0, q = 2, xdata = vp)




# ----------

astsa::acf2(diff(prs), max.lag = 20, main = "Pressure")

astsa::sarima(p = 1, d = 1, q = 1, xdata = diff(prs))

astsa::sarima(p = 1, d = 1, q = 1, xdata = diff(prs), no.constant = TRUE)




# -->
# can not be modelled by ARIMA very well



# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------


# 1st difference of pressure vs. Heater Current
Ccf(diff(film$V3), film$V1[2:351])



# --> 
# First Difference of pressure
#  - has negative correlation with Heat Current, peaks at lag 5,  



# ----------
# 1st difference of pressure vs. Valve Position
Ccf(diff(film$V3), film$V2[2:351])



# --> 
# First Difference of pressure
#  - has posiitive correlation with Valve Position, peaks at lag 4 and 5




# ----------
# Heater Current vs. Valve Position
Ccf(film$V3[2:351], film$V1[2:351])




# ----------
tmp <- data.frame(hc = film$V1[2:351], vp = film$V2[2:351], prsd = diff(film$V3))

Acf(tmp, lag = 20)




# ----------
# 1st difference of pressure vs. Heater Current
astsa::lag2.plot(film$V1[2:351], diff(film$V3), max.lag = 20)



