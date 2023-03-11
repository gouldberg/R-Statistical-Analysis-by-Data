setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\z_for_demo_uncompleted\\q_ungdp_4812")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  q_ungdp_4812
# ------------------------------------------------------------------------------

data <- read.table("q-ungdp-4812.txt", header = TRUE, sep = "")


str(data)



car::some(data)




# ------------------------------------------------------------------------------
# data exploration:  multivariate time series plot
# ------------------------------------------------------------------------------

graphics.off()


MTSplot(data)



plot(data[,4], type = "l", ylab = "")
