setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\z_for_demo_uncompleted\\ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

# data(ozone, package="faraway")

ozone <- read.csv(file = "ozone.txt", header = T, sep = "\t")


str(ozone)




# ------------------------------------------------------------------------------
# Data exploration:  data distribution
# ------------------------------------------------------------------------------


summary(ozone)





# ----------
psych::describe(ozone)




# ----------
car::densityPlot(ozone$O3)
