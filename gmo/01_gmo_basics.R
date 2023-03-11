setwd("//media//kswada//MyFiles//R//gmo")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gmo
#  - Survey on the perception of genetically modified organisms (GMO) of 135 participants conducted by two Argrocampus students.
#    Participants were asked to answer a set of 21 closed questions which were subdivided into two groups,
#    16 questions directly linked to the participants' opinion of GMO and 5 descriptive variables
# ------------------------------------------------------------------------------

gmo <- read.table("gmo.csv", header = TRUE, sep = ";", dec = ".")

dim(gmo)

str(gmo)


car::some(gmo)





