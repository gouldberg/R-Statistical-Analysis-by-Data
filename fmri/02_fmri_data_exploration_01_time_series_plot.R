setwd("//media//kswada//MyFiles//R//fmri1")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fMRI Imaging
# ------------------------------------------------------------------------------

data(fmri1, package = "astsa")

str(fmri1)

head(fmri1)


data(fmri, package = "astsa")

str(fmri)

head(fmri)



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

par(mfrow = c(2,1))

ts.plot(fmri1[,2:5], col = 1:4, ylab = "BOLD", main = "Cortex")

ts.plot(fmri1[,6:9], col = 1:4, ylab = "BOLD", main = "Thalamus & Cerebellum")



# -->
# The series shown are consecutive mesaures of blood oxygenation-level dependent (BOLD) signal intensity,
# which measures areas of activation in the brain.
# Notice that the periodicities appear strongly in the motor cortex series and less strongly in the thalamus and cerebellum.

# The fact that one has series from different areas of the brain suggests testing whether the areas are responding differently to the brush stimulus.



# ------------------------------------------------------------------------------
# data exploration:  time series plot for mean response
# ------------------------------------------------------------------------------

x <- matrix(0, 128, 6)


# We average 
for(i in 1:6){ x[,i] <- rowMeans(fmri[[i]]) }


colnames(x) <- c("Brush", "Heat", "Shock", "Brush", "Heat", "Shock")

plot.ts(x, main = "")

mtext("Awake", side = 3, line = 1.2, adj = 0.05, cex = 1.2)

mtext("Sedated", side = 3, line = 1.2, adj = 0.85, cex = 1.2)



# -->
# Shows the mean response of subjects at Cortex 1 for each of the six treatment combinations.
# Mean responses to various levels of periodic heat, brushing, and shock stimuli for subjects awake and subjects under mild anesthesia.
# 1: Awake-Brush (5 subjects)
# 2: Awake-Heat (4 subjects)
# 3: Awake-Shock (5 subjects)
# 4: Low-Brush (3 subjects)
# 5: Low-Heat (5 subjects)
# 6: Low-Shock (4 subjects)

# The stimuli were periodic in nature, applied alternately for 32s (16 points) and then stopped for 32s.
# The periodic input signal comes through under all three design conditions when the subjects are awake,
# but is somewhat attenuated under anesthesia.

# The mean shock level response hardly shows on the input signal; shock levels were designed to simulate surgical incision without inflicting tissue damage.


