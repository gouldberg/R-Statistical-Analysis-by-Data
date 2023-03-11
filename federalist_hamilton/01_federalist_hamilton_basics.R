setwd("//media//kswada//MyFiles//R//federalist_hamilton")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Federalist Papers  Hamilton
#  - Mosteller and Wallace (1963) give the frequencies, n(k), of count k = 0,1,... of other selected marker words in 247 blocks of text
#    known to have been written by Alexander Hamilton.  The data show the occurrences of the word "upon", that Hamilton used much more
#    than did James Madison
# ------------------------------------------------------------------------------
data <- c(129, 83, 20, 9, 5, 1)
names(data) <- 0:5

data



# ------------------------------------------------------------------------------
# barplot
# ------------------------------------------------------------------------------
k = names(data)

par(mfrow=c(1,1))
b <- barplot(data, names.arg = k, xlab = "Occurrences of 'upon'", ylab = "Number of blocks of text", col = "lightblue", cex.lab = 1.5)



