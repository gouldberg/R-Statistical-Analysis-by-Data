setwd("//media//kswada//MyFiles//R//mytilus")

packages <- c("dplyr", "pixmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mytilus
# ------------------------------------------------------------------------------

M <- read.pnm("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//mytilus.ppm")

M


str(M)


# -----------
plot(M)

