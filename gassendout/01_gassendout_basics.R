setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\gassendout")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Sendout
#   - natural Gas sendout in a southeast region of England, daily from 1st September 1977 to 31st August 1978. (365 days)
#   - Variable:
#   -   gasSendtout (V2):  gas amount consumed except that somes is lost in transmission
#       MetI:  Meteorogical indicator which is designed to help explain and possible predict the Gas sendout
#              The main component of the indicator is temperature, but wind chill is also considered.
# ------------------------------------------------------------------------------


gas <- read.table("GasSendout.txt", sep = "", header = F, colClasses = "numeric")



# ----------
# reverse sign
gas$V1 <- - gas$V1


colnames(gas) <- c("MetI", "gasSendout", "V3")


head(gas)






# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

