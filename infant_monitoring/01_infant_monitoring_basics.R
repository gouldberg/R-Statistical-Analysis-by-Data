setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\infant_monitoring")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Infant Monitoring
#   - Variable:
#       - Blood oxygen saturation, Pulse rate, and Respiration Rate
# ------------------------------------------------------------------------------


infm <- read.table("InfantMon.txt", sep = "", header = F, colClasses = "numeric")



colnames(gas) <- c("MetI", "gasSendout", "V3")


head(infm)




# ----------
colnames(infm) <- c("V1", "bos", "pulr", "respr")




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

