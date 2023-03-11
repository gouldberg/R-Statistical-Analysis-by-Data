setwd("//media//kswada//MyFiles//R//arthritis")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arthritis
# ------------------------------------------------------------------------------
data("Arthritis", package = "vcd")

data <- Arthritis

data



# ------------------------------------------------------------------------------
# Check zero frequency cells
#  - potential impact to logistic regression's predictive performance
# ------------------------------------------------------------------------------
( dat.tab <- xtabs(~ Treatment + Improved + Sex, data = data) )


ftable(dat.tab)
ftable(dat.tab, row.vars = c("Sex", "Treatment"))



# ------------------------------------------------------------------------------
# Plotting conditional distributions of f(Age | Better) as a histtogram, boxplot, or density plot
# ------------------------------------------------------------------------------
with(data, 
     popbio::logi.hist.plot(Age, Improved > "None", 
                            type = "hist", counts = TRUE, ylabel = "Probability (Better)", xlab = "Age", col.hist = "lightblue"))



