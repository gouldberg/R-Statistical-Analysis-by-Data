# setwd("//media//kswada//MyFiles//R//nondurables")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\nondurables")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nondurables
#   - the nondurable goods manufacturing index for the United States.
# ------------------------------------------------------------------------------

# data("nondurables", package = "fda")

nondurables <- read.csv("nondurables.txt", header = TRUE) %>% pull()

nondurables <- ts(nondurables, start = 1919, end = 2000, frequency = 12)


str(nondurables)


# this is time-series data




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

plot(nondurables, ylim=c(0, 120), xlab = 'Year', ylab='Nondurable Goods Index', las=1)



# -->
# There is a tendency for the index to show geometric or exponential increase over the whole century


# ----------
plot(log10(nondurables), xlab = 'Year', 
     ylab=expression(Log[10]~Nondurable~Goods~Index), las=1)

# abline(lm(log10(nondurables) ~ index(nondurables)), lty='dashed')
abline(lm(log10(nondurables) ~ zoo::index(nondurables)), lty='dashed')
abline(v = c(1964, 1968), lty = 2, col = "gray")



# -->
# Plotting the logarithm of the data makes this trend appear linear while giving us a better picture of other types of variation.
# At a finer scale, we see departures from this trend due to the depression, World War II, the end of the Vietnam War
# and other more localized events.

# Moreover, an an even finer scale, there is a marked annual variation, and we can wonder whether this seasonal trend itself shows some longer-term changes.

# Although there are no independent replications here, there is still a lot of repetition of information that we can exploit to obtain
# stable estimates of interesting curve features.

