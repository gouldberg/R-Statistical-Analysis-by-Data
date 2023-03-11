# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/gdp5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  
#  - gdp5:  U.S. GDP over the period of 1984Q1 through 2016Q1
#  - usdata5:  monthly data on the variables inflation, federal funds rate, and bond rate in the U.S. over the period of August 1954 through December 2016
# ------------------------------------------------------------------------------
data("gdp5", package = "POE5Rdata")
data("usdata5", package = "POE5Rdata")


is.ts(gdp5)
is.ts(usdata5)


head(gdp5)
head(usdata5)



# ----------
# convert to ts object
# Note that gdp5 and usdata5 have different frequency (quarterly and monthly)
gdp.ts <- ts(gdp5$gdp, start = c(1984, 1), end = c(2016, 4), frequency = 4)
usa.ts <- ts(usdata5, start = c(1954, 8), end = c(2016, 12), frequency = 12)



# ----------
# create data.frame from ts object
usa.ts.df <- data.frame(b = usa.ts[,2], f = usa.ts[,3], inf = usa.ts[,4])



# ------------------------------------------------------------------------------
# Asess stationarity of federal funds rate by intercept and no trend model
# ------------------------------------------------------------------------------
f <- usa.ts[,"ffr"]

f.dyn <- dynlm(d(f) ~ L(f) + L(d(f)) + L(d(f), 2))

kable(tidy(f.dyn), digis = 4, align = "c", caption = "Checking Federal Funds Rate for Stationarity")



# ------------------------------------------------------------------------------
# Asess stationarity of bond rate by intercept and no trend model
# ------------------------------------------------------------------------------
b <- usa.ts[,"br"]

b.dyn <- dynlm(d(b) ~ L(b) + L(d(b)) + L(d(b), 2))

kable(tidy(b.dyn), digis = 4, align = "c", caption = "Checking 3-year Bond Rate for Stationarity")



# ------------------------------------------------------------------------------
# Asess stationarity of gdp by intercept and "trend" model (coef of gdp(t-1) and unit-root term)
# ------------------------------------------------------------------------------
gdp.ct <- dynlm(d(gdp.ts) ~ trend(gdp.ts) + L(gdp.ts) + L(d(gdp.ts, 1)) + L(d(gdp.ts), 2))

kable(tidy(gdp.ct), digis = 3, align = "c", caption = "GDP Model with Intercept and Trend")


# -->
# insignificant when compared to the Dicey-Fuller critical value



# ------------------------------------------------------------------------------
# Asess stationarity of 1st difference of federal funds rate by no intercept model
# ------------------------------------------------------------------------------
df <- diff(usa.ts.df$f)

df.dyn <- dynlm(d(df) ~ L(df) + d(L(df)) - 1)  # "-1" means no intercept

tidy(df.dyn)



# ------------------------------------------------------------------------------
# Asess stationarity of 1st difference of 3-year Bond Rate by no intercept model
# ------------------------------------------------------------------------------
db <- diff(usa.ts.df$b)

db.dyn <- dynlm(d(db) ~ L(db) + d(L(db)) - 1)

tidy(db.dyn)


