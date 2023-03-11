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
# compare Phase-Plane plots of several years
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow=c(2,2))

phaseplanePlot(1920, logNondurSm4$fd, main = "year 1920")

phaseplanePlot(1930, logNondurSm4$fd, main = "year 1930")

phaseplanePlot(1940, logNondurSm4$fd, main = "year 1940")

phaseplanePlot(1950, logNondurSm4$fd, main = "year 1950")

phaseplanePlot(1960, logNondurSm4$fd, main = "year 1960")

phaseplanePlot(1970, logNondurSm4$fd, main = "year 1970")



# ----------
graphics.off()

par(mfrow=c(2,2))

phaseplanePlot(1980, logNondurSm4$fd, main = "year 1980")

phaseplanePlot(1985, logNondurSm4$fd, main = "year 1985")

phaseplanePlot(1990, logNondurSm4$fd, main = "year 1990")

phaseplanePlot(1995, logNondurSm4$fd, main = "year 1995")
