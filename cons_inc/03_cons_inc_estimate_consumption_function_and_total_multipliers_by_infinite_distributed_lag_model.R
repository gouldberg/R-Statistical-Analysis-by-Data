# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/cons_inc")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cons_inc
#    - y:  permanent income, an infinite stream of income
#    - cons:  consumption
# ------------------------------------------------------------------------------
data("cons_inc", package = "POE5Rdata")

data <- cons_inc

glimpse(data)

str(data)



# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,4)], start = c(1959, 3), end = c(2016, 3), frequency = 4)



# ------------------------------------------------------------------------------
# Consumption function
#  - C(t) = alpha + beta * (Permanent Income)
#         = alpha + beta0 * y(t) + beta1 * y(t-1) + ... + e(t)
#         --> This is Infinite Distribution Lag Model (IDL)
#
#  - This IDL model can be transformed into ARDL(1,0) model
#  - D(C(t)) = delta + lambda * D(C(t-1)) + beta0 * D(y(t)) + v(t)
#         delta = alpha * (1 - lambda)     sum(beta) = beta0 * lambda ^ s = beta0 / ( 1 - lambda )    v(t) = e(t) - lambda * e(t-1)    0 < lambda < 1
# ------------------------------------------------------------------------------

# Consumption Function
modC <- dynlm(d(cons, 1) ~ L(d(cons, 1)) + d(y, 1), data = data.ts)

tidy(modC)


# -->
# lambda = 0.33690 and beta0 = 0.09908 --> beta0 / ( 1 - lamda ) = 0.1494194 = total sum of beta



# ----------
# Calculate Delay multipliers, Interim multipliers and total multiplier
multipliers <- function(model, M){
  mdel <- numeric(M)  # creates vector of delay multipliers
  mint <- numeric(M)  # creates vector of interim multipliers
  delta <- coef(model)[[1]]
  lambda <- coef(model)[[2]]
  b0 <- coef(model)[[3]]
  for(s in 0:(M-1)){ mdel[s+1] <- b0 * lambda^s }
  for(s in 0:(M-1)){ mint[s+1] <- b0 * (1 - lambda^(s+1)) / (1 - lambda) }
  mtot <- b0 / ( 1 - lambda)
  lst <- list(mdel, mint, mtot)
}


M <- 10  # the number of delay or interim multipliers desired
mList <- multipliers(modC, M)

tab <- data.frame(mList[[1]], mList[[2]], mList[[3]])

names(tab) <- c("Delay Multipliers", "Interim Multipliers", "Total Multiplier")

kable(tab, digits=3, align="c", caption = "Multipliers in Infinte Lag Model")


# -->
# Delay Multipliers:  Coefficient of lags
# Impact Multipliers:  the order 0 coefficient, the immediate (contemporaneous) impact of a change in x on y
# Interim Multipliers:  If x increase by one unit today, the change in y will be beta0 + beta1 + ... + betas after s periods

# If changes in d(y, 1) in one unit at current quarter, 
# the change in d(cons, 1) will be 0.149 after 5 quarters


