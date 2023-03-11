setwd("//media//kswada//MyFiles//R//fmri1")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fMRI Imaging
#   - data collected from various locations in the brain via functional magnetic resonance imaging (fMRI).
#     Five subjects were given periodic brushing on the hand. The stimulus was applied for 32 seconds and then stopped for 32 seconds;
#     Thus, the signal period is 64 seconds. The sampling rate was one observation every 2 seconds for 256 seconds (n = 128).
#   - fmri1:  the data are averaged over subjects (evoked responses, and all subjects were in phase)
#
#   - The periodic input signal comes through under all 3 design conditions when the subjects are awake, but is somewhat attenuated under anesthesia
#   - Shock levels are designed to simulate surgical incision without inflicting tissue damage.
#
#   - The blood oxygenation level (BOLD) signal intensity was measured at 9 locations ("L1" to "L9") in the brain for subject.
#     Areas of activation were determined using a technique first described by Bandettini et al.
#     Specific locations of the brain where the signal was measured were
#      - Cortex1:  Primary Somatosensory, Contralateral
#      - Cortex2:  Primary Somatosensory, Ipsilateral
#      - Cortex3:  Secondary Somatosensory, Contralateral
#      - Cortex4:  Secondary Somatosensory, Ipsilateral
#      - Caudate
#      - Thalamus1:  Contralateral
#      - Thalamus2:  Ipsilateral
#      - Cerebellum1:  Contralateral
#      - Cerebellum2:  Ipsilateral
# 
#   - This is the designed experiment measuring !!!
# ------------------------------------------------------------------------------

data(fmri1, package = "astsa")

str(fmri1)

head(fmri1)


data(fmri, package = "astsa")

str(fmri)

head(fmri)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

