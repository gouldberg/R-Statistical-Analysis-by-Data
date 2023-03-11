setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\infant_monitoring")

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
