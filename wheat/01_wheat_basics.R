setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
#   - The presence of sprouted or diseased kernels in wheat can reduce the value of a wheat producer's entire crop.
#     It is important to identify these kernels after being harvested but prior to sale. To facilitate this identification process,
#     automated systems have been developed to separate healthy kernels from the rest.
#     Improving these systems requires better understanding of the measureable ways in which healthy kernels differ from kernels that
#     have sprouted prematurely or are infected with afungus ("Scrab").
#   - To this end, Martin et al. (1998) conducted a study examining numerous physical properties of kernels -- density, hardness, size, weight,
#     and moisture content -- measured on a sample of wheat kernels from two different classes of wheat, hard red winter (hrw) and soft red winter (srw).
#     Each kernel's condition was also classified as "Healthy", "Sprout", or "Scab" by human visual inspection.
#   - In the data provided by the authors of this paper, we have measurements from 275 wheat kernels. Our data is only a portion of the data
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)




# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

psych::describe(wheat)

psych::pairs.panels(wheat)
