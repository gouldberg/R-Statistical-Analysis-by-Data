# setwd("//media//kswada//MyFiles//R//nondurables")
setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\nondurables")

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
# basics
# ------------------------------------------------------------------------------