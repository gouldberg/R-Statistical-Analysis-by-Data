
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
#   - The ARMD data arise from a randomized multi-center clinical trial comparing an experimental treatment
#     (interferon-alpha) versus placebo for patients diagnosed with ARMD.
#   - Patients with macular degeneration progressively lose vision.
#     In the trial, visual acuity of each of 240 patients was assessed at baseline and at four post-randomization timepoints,
#     i.e., at 4, 12, 24, and 52 weeeks.
#     Visual acuity was evaluated based on patient's ability to read lines of letters on standardized vision charts.
#     The charts display lines of five letters of decreasing size, which the patient must read from top (largeest letters)
#     to bottom (smallest letters). Each line with at least four letters correctly read is called one "line of vision".
#     In our analysis, we will focus on the visual acruity defined as the total number of letters correctly read.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# data:  armd240.data
# ------------------------------------------------------------------------------

armd240.data <- read.csv("armd240.data.txt", header = T, sep = "\t")


str(armd240.data)


car::some(armd240.data)



# ------------------------------------------------------------------------------
# data:  armd.wide
# ------------------------------------------------------------------------------

# library(nlmeU)
# data(armd.wide, package = "nlmeU")


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


str(armd.wide)


car::some(armd.wide)



# -->
# miss.pat:  containing a missing-pattern identifier, i.e., a character string that indicates which of the four
# post-randomization measurements of visual acuity are missing for a particular patient.
# The missing values are marked by X.



# ----------
# missing patterns:

attach(armd240.data)

( miss.pat <- nlmeU:::missPat(visual12, visual12, visual24, visual52) )


detach(armd240.data)



# ------------------------------------------------------------------------------
# data:  armd0  (long format data)
# ------------------------------------------------------------------------------


# library(nlmeU)
# data(armd0, package = "nlmeU")

armd0 <- read.csv("armd0.txt", header = T, sep = "\t")


str(armd0)


car::some(armd0)




# ---------
# post-baseline measures

auxDt <- subset(armd0, time > 0)


levels(auxDt$time.f)


armd <- droplevels(auxDt)


levels(armd$time.f)


# we assign orthogonal polynomial contrasts to the factor time.f
armd <- within(armd, {
  contrasts(time.f) <- contr.poly(4, scores = c(4, 12, 24, 52))
})


head(armd)


str(armd)



