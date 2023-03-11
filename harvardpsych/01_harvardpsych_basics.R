setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HarvardPsych
#   - In the case of the Harvard Psychology faculty, every faculty member provides a short research statement on the department website.
#     The dataset is DTM (document-term matrix), a cell entry denotes how often a particular researcher mentioned a particular keyword in his/her
#     research statement.
# ------------------------------------------------------------------------------

data("HarvardPsych", package = "MPsychoR")

str(HarvardPsych)

dim(HarvardPsych)


# researchers in rows, words in columns  (29 * 43)
head(HarvardPsych)



# ----------
rownames(HarvardPsych)

colnames(HarvardPsych)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
