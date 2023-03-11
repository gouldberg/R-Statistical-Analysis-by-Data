# setwd("//media//kswada//MyFiles//R//soi_rec")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soiltemp
#    - 64 * 36 spatial grid set out on an agricultural field.
#      The value is the temperature measured at each grid
# ------------------------------------------------------------------------------

data(soiltemp, package = "astsa")


dim(soiltemp)




# ------------------------------------------------------------------------------
# plot the temeratures on each grid
# ------------------------------------------------------------------------------


MTSplot(soiltemp)



# ----------
par(mfrow = c(2,1))

# means at each row
plot.ts(rowMeans(soiltemp), xlab ="row", ylab = "Average Temperature")


# means at column
plot.ts(colMeans(soiltemp), xlab ="column", ylab = "Average Temperature")



# ----------
# plot in 3D

par(mfrow = c(1,1))

persp(1:64, 1:36, soiltemp,
      phi = 25, theta = 25, scale = FALSE, expand = 4,
      ticktype = "detailed", 
      xlab = "rows", ylab = "cols", zlab = "temperature")




# ------------------------------------------------------------------------------
# 2 dimensional autocorrelation function
# ------------------------------------------------------------------------------


dft <- fft(soiltemp - mean(soiltemp))


fs <- abs(dft)^2 * 4 / (64 * 36)



# ----------
# 2-dimensional autocovariance function
cs <- Re(fft(fs, inverse = TRUE)) / sqrt((64 * 36))



# ----------
# 2-dimensional auto-correlation function
rs <- cs / cs[1,1]


( rs2 <- cbind(rs[1:41, 21:2], rs[1:41, 1:21]) )


( rs3 <- rbind(rs2[41:2,], rs2) )





# ----------
# plot 2-dimensional auto-correlation function

graphics.off()

par(mfrow = c(1,1))

persp(-40:40, -20:20, rs3, phi = 30, theta = 30, expand = 30, scale = "FALSE",
      ticktype = "detailed", xlab = "row lags", ylab = "column lags",
      zlab = "ACF")




# ------------------------------------------------------------------------------
# 2 dimensional periodogram
# ------------------------------------------------------------------------------


( fs2 <- cbind(fs[1:32, 18:2], fs[1:32, 1:18]) )


( fs3 <- rbind(fs2[32:2,], fs2) )



# ----------
# maximum value

max(fs)


which(fs == max(fs))


# 0.0625 cylces per row
(5-1) / 64




# ----------
# plot 2-dimensional periodogram

graphics.off()

par(mfrow = c(1,1))

persp((-31:31)/64, (-17:17)/36, fs3, phi = 30, theta = 30, expand = 0.6, scale = "TRUE",
      ticktype = "detailed", xlab = "cycles/row", ylab = "cycles/column",
      zlab = "Periodogram Ordinate")




# -->
# showing peak at 0.0625 cycles/row
# period is 16 rows, 1 row is 17ft  -->  16 * 17 ft = 272 ft period / cycle

1 / 0.0625



# ----------
# periodogram of soiltemp, individual by rows

mvspec(soiltemp, log = "no")




