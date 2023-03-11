setwd("//media//kswada//MyFiles//R//federalist")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Federalist Papers
#  - In 1787-1788, Alexander Hamilton, John Jay, and James Madison wrote a series of newspaper essays to persuage the votes of
#    New York State to ratify the U.S. Constitution.
#    The essayes were titled "The Federalist Papers" and all were signed with the pseudonym "Publius."
#    Of the 77 papers published, the author(s) of 65 are known, but both Hamilton and Madison later claimed sole authorship of the remaining 12.
#    Mosteller and Wallace (1963, 1984) investigated the use of statistical methods to identify authors of disputed works
#    based on the frequency distributions of certain key function words, and concluded that Madison had indeed authored the 12 disputed papers.
#
#    The data shows the distribution of the occurrence of one of these "marker" words, the word "may" in 262 blocks of text (each about 200 words long)
#    from issues of the Federalist Papers and other essays known to be written by James Madison.
# ------------------------------------------------------------------------------

data("Federalist", package = "vcd")


data <- Federalist


data

sum(data)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

