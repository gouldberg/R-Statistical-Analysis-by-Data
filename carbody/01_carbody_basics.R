
setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\carbody")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  car body coating process data
# ------------------------------------------------------------------------------

dat <- read.csv("carbody_dat.txt", header = TRUE, skip = 2, sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


