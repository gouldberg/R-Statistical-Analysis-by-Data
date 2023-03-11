setwd("//media//kswada//MyFiles//R//bnrf1ebv")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bnrf1ebv
#   - Nucleotide sequence of the BNRF1 gene of the Epstein-Barr virus (EBV): 1=A, 2=C, 3=G, 4=T.
#     A strand of DNA can be represented as a sequence of letters, termed base pairs (bp), from the finite alphabet {A, C, G, T}.
#     The order of the nucleotides contains the genetic information specific to the organism.
#   - The expression of infromation stored in these molecules is a complex multistage process.
#     One important task is to translate the information sotred in the protein-coding sequences (CDS) of the DNA.
#     A common problem in analyzing long DNA sequence data is in identifying CDS dispersed throughout the sequence and separated by regions of noncoding
#     (which makes up most of the DNA).
# ------------------------------------------------------------------------------

data(bnrf1ebv, package = "astsa")

bnrf1ebv



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

