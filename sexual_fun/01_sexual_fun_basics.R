setwd("//media//kswada//MyFiles//R//sexual_fun")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Sexual Fun
#  - This data, from Hout et al. (1987) summarizes the responses of 91 married couples to a questionnaire item:
#    "Sex is fun for me and my partner: (a) Never or occasionally", (b) Fairly often, (c) Very often, (d) Almost always"
# ------------------------------------------------------------------------------
data("SexualFun", package = "vcd")

data <- SexualFun

data


# ----------
sieve(data, shade=TRUE)

# --> In each row the diagonal entry is not always the largest, though it appears that the partners tend to agree more often
# either responds "Almost always"
