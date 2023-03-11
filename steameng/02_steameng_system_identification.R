setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\steameng")

packages <- c("sysid", "control")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SteamEng
#   - Input:  Pressure and MagVolt
#   - Output: GenVolt, Speed
# ------------------------------------------------------------------------------


data <- read.table(file = "SteamEng.txt", header = T, sep = "\t", stringsAsFactors = FALSE)


str(data)


car::some(data)



# ----------
pressure <- ts(data$pressure)

magvolt <- ts(data$magvolt)

genvolt <- ts(data$genvolt)

speed <- ts(data$speed)



# ----------
# convert to idframe
z <- idframe(output = data.frame(genvolt, speed), input = data.frame(magvolt, pressure), Ts = 1)
z1 <- idframe(output = data.frame(genvolt), input = data.frame(magvolt), Ts = 1)
z2 <- idframe(output = data.frame(genvolt), input = data.frame(pressure), Ts = 1)
z3 <- idframe(output = data.frame(speed), input = data.frame(magvolt), Ts = 1)
z4 <- idframe(output = data.frame(speed), input = data.frame(pressure), Ts = 1)


colnames(z$output) <- c("genvolt", "speed")

colnames(z$input) <- c("magvolt", "pressure")




# ------------------------------------------------------------------------------
# time series plot
# ------------------------------------------------------------------------------


str(z)


# split data
ze <- dataSlice(z, start = 1, end = 350)
ze1 <- dataSlice(z1, start = 1, end = 350)
ze2 <- dataSlice(z2, start = 1, end = 350)
ze3 <- dataSlice(z3, start = 1, end = 350)
ze4 <- dataSlice(z4, start = 1, end = 350)

zr <- dataSlice(z, start = 351, end = 451)
zr1 <- dataSlice(z1, start = 351, end = 451)
zr2 <- dataSlice(z2, start = 351, end = 451)
zr3 <- dataSlice(z3, start = 351, end = 451)
zr4 <- dataSlice(z4, start = 351, end = 451)



# ----------
plot(z)



par(mfrow = c(2,1))

plot(ze)




# ------------------------------------------------------------------------------
# Estimate Frequency Response
# ------------------------------------------------------------------------------


# default lag size of the Hanning window (Default: min (length(x)/10,30))
# frequency points at which the response is evaluated (Default: seq(1,128)/128*pi/Ts --> up to 3.14)
sp <- spa(ze)


# bode plot
#  - Y: amplitude in decibel, phase in degree
#  - X: rad / unit time, 1 means 1 rad/unit time
plot(sp)




# ------------------------------------------------------------------------------
# Impulse Response Function
# ------------------------------------------------------------------------------


fit <- impulseest(ze1, M = 40)
fit <- impulseest(ze2, M = 40)
fit <- impulseest(ze3, M = 40)
fit <- impulseest(ze4, M = 40)


sysid::impulseplot(fit)





# ----------
# step response function

st <- sysid::step(fit)

st$data




