setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\film_process")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Film Extrusion Process
# ------------------------------------------------------------------------------


film <- read.table("FilmProcess.txt", sep = "", header = F, colClasses = "numeric")


head(film)



# ----------
hc <- film$V1

vp <- film$V2

prs <- film$V3

wid <- film$V4




# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram of 1st differenced series
# ------------------------------------------------------------------------------


nextn(length(hc))



# ----------
par(mfrow=c(2,1))


hc.per <- astsa::mvspec(hc, log = "no")


vp.per <- astsa::mvspec(vp, log = "no")


# prs.per <- astsa::mvspec(diff(prs), log = "no")
prs.per <- astsa::mvspec(prs, log = "no")


wid.per <- astsa::mvspec(wid, log = "no")



# ----------
head(sort(vp.per$spec, decreasing = T))


which(vp.per$spec >= 500000)


vp.per$freq[c(3,9,11,45,68)]



nextn(length(vp)) * 0.00833

nextn(length(vp)) * 0.025

nextn(length(vp)) * 0.03056

nextn(length(vp)) * 0.1888




