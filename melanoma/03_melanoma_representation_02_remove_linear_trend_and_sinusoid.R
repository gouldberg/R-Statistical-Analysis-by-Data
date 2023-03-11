# setwd("//media//kswada//MyFiles//R//melanoma")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\melanoma")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  melanoma
# ------------------------------------------------------------------------------

melanoma <- t(matrix(scan("melanoma.txt", 0), 3, 37)) %>% as.data.frame() %>% dplyr::select(V2, V3)

# data("melanoma", package = "fda")


colnames(melanoma) <- c("year", "incidence")



str(melanoma)




# ------------------------------------------------------------------------------
# Remove linear trend and sinusoid
# ------------------------------------------------------------------------------

year  <- melanoma[,1]

mela  <- melanoma[,2]

nyear <- length(year)

yearRng = c(1936, 1972)



# ----------
# stats::lsfit() find the least sqaures fit
# remove a linear trend

melahat1 <- mela - lsfit(x = year, y = mela)$residual

sse1 <- sum((mela - melahat1)^2)




# ----------
# remove linear trend and sinusoid
# 2 * pi / 9.67 = 0.649

xmat2 <- cbind(year, sin(0.65 * year))

xmat2


plot(xmat2, type = "o")



melahat2 <- mela - lsfit(xmat2, mela)$residual

sse2 <- sum((mela - melahat2)^2)



# ----------
# remove a linear trend at log10 base
lnmela <- log10(mela)

lnmelahat1 <- lnmela - lsfit(year, lnmela)$residual

sse1_l <- sum((lnmela - lnmelahat1)^2)



# ----------
# remove linear trend and sinusoid at log10 base
lnmelahat2 <- lnmela - lsfit(xmat2, lnmela)$residual

sse2_l <- sum((lnmela - lnmelahat2)^2)



# ------------------------------------------------------------------------------
# Plot 
# ------------------------------------------------------------------------------

graphics.off
par(mfrow=c(1,2), mar=c(5,5,4,2), pty="m")

plot(year, mela, type="p", cex=1, xlab="Year", ylab="Cases per 100,000")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")
lines(year, melahat1, lty=4)
lines(year, melahat2, lty=1)


plot(year, lnmela, type="p", cex=1, ylim=c(-0.1,0.8), xlab="Year", ylab="Log_10 Cases per 100,000")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")
lines(year, lnmelahat1, lty=4)
lines(year, lnmelahat2, lty=1)



# ----------
sse1
sse2

sse1_l
sse2_l


