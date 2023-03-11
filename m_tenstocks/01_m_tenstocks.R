# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
#   - monthly log returns of stocks for ten U.S. companies from January 2001 to December 2011 for 132 observations
#     The ten companies can roughly be classified into 3 industrial sectors, namely, semiconductor, pharmaceutical,
#     and investment banks
#   - The mean returns of the ten stocks are all close to 0, but the log returns have some negative skewness and high excess kurtosis.
#
#   - Semiconductor
#        - TXN:  Texas instru.
#        - MU:  Micron Tech.
#        - INTC:  Intel Corp.
#        - TSM:  Taiwan Semi.
#   - Pharmaceutical
#        - PFE:  Pfizer
#        - MRK:  Merck & Co.
#        - LLY:  Eli Lilly
#   - Investment Bank
#        - JPM:  JPMorgan hase
#        - MS:  Morgan Stanley
#        - GS:  Goldman Sachs
# ------------------------------------------------------------------------------

mtenstocks <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(mtenstocks)


dim(mtenstocks)


car::some(mtenstocks)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
