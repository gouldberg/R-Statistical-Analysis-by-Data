setwd("//media//kswada//MyFiles//R//federalist")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Federalist Papers
# ------------------------------------------------------------------------------

data("Federalist", package = "vcd")


data <- Federalist


data

sum(data)





# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

k = names(data)

par(mfrow=c(1,1))


b <- barplot(data, names.arg = k, xlab = "Occurrences of 'may'", ylab = "Number of blocks of text", col = "lightblue", cex.lab = 1.5)



