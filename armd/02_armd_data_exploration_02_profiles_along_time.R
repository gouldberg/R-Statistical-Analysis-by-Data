
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)




# ------------------------------------------------------------------------------
# Data exploration:  profiles along time, Active vs. Placebo
# ------------------------------------------------------------------------------


library(lattice)


armd0.subset <- subset(armd0, as.numeric(subject) %in% seq(1, 240, 10))



xy1 <- xyplot(visual ~ jitter(time) | treat.f, 
              groups = subject,
              data = armd0.subset,
              type = "l", lty = 1)


update(xy1, xlab = "Time (in weeks)", ylab = "Visual acuity", grid = "h")





