# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\cpi")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CPI, EXR
#   - CPI (Consumer Price Index), Exchange Rage
# ------------------------------------------------------------------------------

dat <- read.csv("cpi_exr_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
