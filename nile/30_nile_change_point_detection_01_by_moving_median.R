rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------


data(Nile)


dim(Nile)


str(Nile)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


ts.plot(Nile)




# ------------------------------------------------------------------------------
# moving average and moving median
# ------------------------------------------------------------------------------


z <- c(Nile)

y <- z


ndata <- length(z)


y[1:ndata] <- NA



# ----------
graphics.off()

par(mfrow = c(2,1))

plot(z)


# moving average:  span = 9 and 3
lines(c(stats::filter(z, rep(1/9,9), side = 2)), lwd = 2, col = "blue")

lines(c(stats::filter(z, rep(1/3,3), side = 2)), lwd = 2, col = "red")




# ----------
# moving medians:  span = 9 and 3

plot(z)

kfilter <- 9
n0 <- kfilter + 1
n1 <- ndata - kfilter

for(i in n0:n1){
  i0 <- i - kfilter
  i1 <- i + kfilter
  y[i] <- median(z[i0:i1])
}

lines(y, col = "blue", lwd = 2)


kfilter <- 3
n0 <- kfilter + 1
n1 <- ndata - kfilter

for(i in n0:n1){
  i0 <- i - kfilter
  i1 <- i + kfilter
  y[i] <- median(z[i0:i1])
}

lines(y, col = "red", lwd = 2)




