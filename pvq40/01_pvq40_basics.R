setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
#   - A more recent example of using MDS to test structural hypotheses is a study by Borg et al. (2017).
#     They asked 151 US adults to answer the PVQ40, a questionnaire that measures the personal importance of ten basic values:
#       - PO = power, AC = achievement, HE = hedonism, ST = stimulation, SD = self-direction, UN = universalism, BE = benevolence,
#         TR = tradition, CO = conformity, and SE = security
#     The PVQ consists of 40 items, each a short portrait of one person. Each portrait describes a person's goals, aspirations, and desires that
#     reflect that person's values.
#     Participants rate the extent to which each person portrayed is similar to themselves, using a 6-point response scale ranging from
#     "not like me at all" (0) to "very much like me" (6)
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")


str(PVQ40)


attributes(PVQ40)


car::some(PVQ40)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


