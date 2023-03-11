grasetwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\gassendout")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Sendout
# ------------------------------------------------------------------------------

gas <- read.table("GasSendout.txt", sep = "", header = F, colClasses = "numeric")



colnames(gas) <- c("MetI", "gasSendout", "V3")


head(gas)




# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram of 1st differenced series
# ------------------------------------------------------------------------------

# input <- gas$MetI
# output <- gas$gasSendout


# 1st order differencing
input <- diff(gas$MetI)
output <- diff(gas$gasSendout)



nextn(length(input))



# ----------
par(mfrow=c(2,1))


input.per <- astsa::mvspec(input, log = "no")

output.per <- astsa::mvspec(output, log = "no")




# ----------
head(sort(output.per$spec, decreasing = T))


which(output.per$spec >= 8500)


output.per$freq[c(53,54,107)]



# this is weekly and half-weekly cycles
nextn(length(input)) * 0.1413

nextn(length(input)) * 0.2853




# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram for smoothed data
# ------------------------------------------------------------------------------


# take residuls by smooth.spline
input <- gas$MetI - smooth.spline(time(gas$MetI), gas$MetI, spar = 1.0)$y

output <- gas$gasSendout - smooth.spline(time(gas$gasSendout), gas$gasSendout, spar = 1.0)$y




nextn(length(input))



# ----------
par(mfrow=c(2,1))


input.per <- astsa::mvspec(input, log = "no")

output.per <- astsa::mvspec(output, log = "no")




# ----------
head(sort(output.per$spec, decreasing = T))

which(output.per$spec >= 79280)


head(sort(input.per$spec, decreasing = T))

which(input.per$spec >= 25000)


output.per$freq[c(5)]

input.per$freq[c(5)]



# almost 5 times cycle in a year
nextn(length(input)) * 0.0133


