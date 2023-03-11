setwd("//media//kswada//MyFiles//R//catt")

packages <- c("dplyr", "fda", "refund")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CATT
#   - Data of Comparison of Age-Related Macular Degeneration Treatments Trails (CATT),
#     which was a multi-centered randomized clinical trial to assess the relative safety and efficacy of two treatments
#     for subfoveal neovascular Age-Related Macular Degeneration (AMD), a disease that usually causes severe irreversible vision loss.
#     CATT was conducted in 44 clinical centers and was funded by the National Eye Institute.
#   - The data can be downloaded from http://www.med.upenn/edu/cpob/studies/CATT.shtml.
#
#   - Each subject made clinical visits every 4 weeks with up to 27 total visits (including a baseline visit).
#     After their initial visit, subjects were randomly assigned to one of four treatment groups involving different administrations of the drugs
#     Lucentis and Avastin.
#
#   - The primary outcome we considere here is Visual Acuity Score, VAS.
#     A VAS of 0.5 is 20 / 40 vision, i.e., the subject reads at 20 feet, what a person with sound eyesight could read at 40 feet.
#     Thus, the lower the VAS, the worse a subject's vision.
#     A subject is considered legally blind when their VAS is less than 0.1 or 20 / 200, while a VAs of 0 is actually blind.
#   - While the study design is balanced (all subjects are observed at the same times), there are many missing values.
#     Only 19% of subjects are completely observed, while 54% are missing 1-3 observations.
# ------------------------------------------------------------------------------

# VAS Longitudinal Measurement
Data <- read.csv("va.csv")


# VAS Baseline Measurements
Base <- read.csv("bv.csv")


# Other Subject Information
general_info <- read.csv("gi.csv")


str(Data)

str(Base)

str(general_info)



# ----------
car::some(Data)

car::some(Base)

car::some(general_info)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

( Code <- unique(Base$alpha_code) )



# ----------
# sample and plot visual acuity vs. time
( samp_code <- sample(length(Code), size = 16, replace = FALSE) )


graphics.off()
par(mfrow = c(4,4), mar = c(2,2,2,2))
for(i in 1:16){
  dat <- Data %>% filter(alpha_code %in% Code[samp_code[i]]) %>% dplyr::select(week, studyeye_va)
  plot(studyeye_va ~ week, data = dat, type = "b", xlab = "week", ylab = "Visual Acuity", main = paste0(Code[i]))
}


