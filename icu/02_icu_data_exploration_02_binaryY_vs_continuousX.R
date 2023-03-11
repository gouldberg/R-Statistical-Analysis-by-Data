setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots
# binary Y vs. continuous X
# ------------------------------------------------------------------------------

# Margianl plots to check relationship between y and x

var_y <- "died"

var_x <- c("age", "systolic", "hrtrate")



# ----------
op <- par(mfrow(c(2,2), mar = c(4,4,1,2.5) + .1 , cex.lab = 1.4))

col <- c("skyblue", "blue")

par(mfrow = c(3,1))

for(i in 1:length(var_x)){
  eval(parse(text = paste0("plot(", var_y, " ~ ", var_x[i], ", data = ICU, col = col)")))
}







