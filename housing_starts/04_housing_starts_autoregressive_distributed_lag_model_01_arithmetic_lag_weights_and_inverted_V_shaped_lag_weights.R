# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/housing_starts")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: housing starts  
# ------------------------------------------------------------------------------

starts <- c(1252, 1313, 1463, 1610, 1529, 1473, 1165, 1292, 1508, 1467,
            1434, 2052, 2357, 2045, 1338, 1160, 1538, 1987, 2020, 1745,
            1292, 1084, 1062, 1703, 1750, 1742, 1805, 1621, 1488, 1376,
            1193, 1014, 1200, 1288, 1457, 1354, 1477, 1474, 1617, 1641,
            1569, 1603, 1705, 1848, 1956, 2068)

sales <- c(485, 656, 718, 634, 519, 549, 646, 819, 817, 709, 545,
           436, 412, 623, 639, 688, 750, 671, 676, 650, 534, 509,
           610, 666, 670, 667, 757, 804, 886, 880, 877, 908, 973, 1086, 1203, 1283)


# ----------
# Note that sales data is leading 10 years
length(starts)
length(sales)



# ----------
# add NA to first 10 years to sales
sales2 <- c(rep(NA, 10), sales)
length(sales2)



# ----------
data <- data.frame(starts = starts, sales2 = sales2)

data.ts <- ts(data, start = c(1960), end = c(2005), frequency = 1)



# ------------------------------------------------------------------------------
# Autoregressive Distributed Lag Model:  Arithmetic Lag
#   - beta(i) = ( K + 1 - i ) * beta
#     where the lags beta(i) decline from beta to zero on a straight line as i increase from 1 to k
# ------------------------------------------------------------------------------

# function to compute sum [reverse(w) * x(t-j)] over j = 1 to length(w)
lagwt <- function(x, w){

  x2 = na.omit(x)
  print("missing data areassumed to be at the beginning and simply omitted")
  
  n = length(x2);  m = length(w);  nm = n - m;  out = rep(NA, nm)
  
  for(i in 1:nm){
    sumout = x[(1 + i - 1):(i - 1 + m)] * rev(w)
    out[i] = sum(sumout)
  }
  return(out)  
}


# ----------
# Simulate various k
for(k in 3:10){
  ii = 1:k;  bet = 1;  Arith = (k + 1 - ii) * bet;
  start2 = lagwt(starts, Arith)
  
  n = length(sales); nn = length(start2)
  starts2 = start2[(nn - n + 1):nn]
  reg1 = lm(sales ~ starts2)
  rsq2 = (cor(fitted(reg1), sales))^2
  print(paste0("k = ", k, "  :  rsq2 = ", round(rsq2, 4)))
}



# ----------
# we choose k = 3, where rsq2 is largest, but still miserably low
k = 3
ii = 1:k;  bet = 1;  Arith = (k + 1 - ii) * bet;

graphics.off()
par(mfrow = c(1,1))
plot(ii, Arith, type = "l", main = "Arithmetic Lag Weights")


# ----------
start2 = lagwt(starts, Arith)
n = length(sales); nn = length(start2)
starts2 = start2[(nn - n + 1):nn]
reg1 = lm(sales ~ starts2)
summary(reg1)



# ------------------------------------------------------------------------------
# Autoregressive Distributed Lag Model:  Inverted V lag
#   - beta(i) = i * beta     for i = [0, k/2] and
#     beta(i) = (k - i) * beta      for i (k/2, k) 
#   - The plot of beta(i) agains i goes up and comes down
# ------------------------------------------------------------------------------

# Simulate various k
# k needs to be an even number here
for(k in seq(2, 10, 2)){
  i = 1:(k/2);  bet = 1;  InvV1 = i * bet;  # bet will be estimated
  i = (1 + (k/2)):k;  InvV2 = (k - i) * bet;
  InvV = c(InvV1, InvV2)  # combine two parts of inverted V
  
  start2 = lagwt(starts, InvV)
  n = length(sales); nn = length(start2)
  starts2 = start2[(nn - n + 1):nn]
  reg1 = lm(sales ~ starts2)
  rsq2 = (cor(fitted(reg1), sales))^2
  print(paste0("k = ", k, "  :  rsq2 = ", round(rsq2, 4)))
}


# ----------
# we choose k = 2, where rsq2 is largest, but still miserably low
k = 2  # k = 2 will not be V shaped !!!
# k = 4  --> k = 4 will be V shaped
i = 1:(k/2);  bet = 1;  InvV1 = i * bet;
i = (1 + (k/2)):k;  InvV2 = (k - i) * bet;
InvV = c(InvV1, InvV2)

graphics.off()
par(mfrow = c(1,1))
plot(1:k, InvV, type = "l", main = "Inverted V shaped Lag Weights")


# ----------
start2 = lagwt(starts, InvV)
n = length(sales); nn = length(start2)
starts2 = start2[(nn - n + 1):nn]
reg2 = lm(sales ~ starts2)
summary(reg2)


# -->
# This has negative adjusted Rsquare.


