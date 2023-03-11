
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
# ------------------------------------------------------------------------------


gasf <- read.table("GasFurnace.txt", sep = "", header = F, colClasses = "numeric")


colnames(gasf) <- c("input", "output")


head(gasf)




# ----------
# VAR model

library(vars)


input <- gasf$input

output <- gasf$output


x <- cbind(input, output)

fit3 <- VAR(x, p = 3, type = "both")

fit6 <- VAR(x, p = 6, type = "both")




# ------------------------------------------------------------------------------
# Granger causality testing by vars::causality()
# ------------------------------------------------------------------------------

# Granger causality H0:  "input" do not Granger-cause "output" --> REJECTED
# H0: No instantaneous causality between input and output --> NOT REJECTED


vars::causality(fit6, cause = "input")




# ----------
# Granger causality H0:  "output" do not Granger-cause "input" --> NOT REJECTED
# H0: No instantaneous causality between output and input --> NOT REJECTED

x2 <- cbind(output, input)

fit6_2 <- VAR(x2, p = 6, type = "both")

summary(fit6_2)


vars::causality(fit6_2, cause = "output")

