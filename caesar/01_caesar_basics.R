setwd("//media//kswada//MyFiles//R//caesar")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Caesar
#   - 3 * 2^3 frequency table classifying 251 women who gave birth by Caesarian section by Infection (three levels: none, Type 1, Type 2)
#     and Risk, whether Antibiotics were used, and whether the Caesarian section was Planned or not.
# ------------------------------------------------------------------------------
data("Caesar", package = "vcdExtra")

Caesar


# Convert to data frame
data <- as.data.frame(Caesar)


# Here consider only the binary outcome of infection vs. no infection
data$Infect <- as.numeric(data$Infection %in% c("Type 1", "Type 2"))


data








