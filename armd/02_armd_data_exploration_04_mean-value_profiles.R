
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
# Data exploration:  mean-value profiles
# ------------------------------------------------------------------------------

# counts of non-missing visual acuity measurements

attach(armd0)


( flst <- list(time.f, treat.f) )


( tN <- tapply(visual, flst, FUN = function(x) length(x[!is.na(x)])) )




# ----------
# sampe means and medians of visual acuity measurement


tMn <- tapply(visual, flst, FUN = mean)


tMd <- tapply(visual, flst, FUN = median)


colnames(res <- cbind(tN, tMn, tMd))


nms1 <- rep(c("P", "A"), 3)


nms2 <- rep(c("n", "Mean", "Mdn"), rep(2, 3))


colnames(res) <- paste(nms1, nms2, sep = ":")


res


detach(armd0)



# -->
# we conclude that, on average, there was very little difference in visual acuity between the two treatment groups
# at baseline.
# This is expected in a randomized study.

# During the course of the study, the mean visual acuity decreased with time in both arms,
# which confirms the observation made based on the individual profiles.

# It is worth noting that the mean value is consistently higher in the placebo group,
# which suggests lack of effect of interferon-alpha




# ------------------------------------------------------------------------------
# Data exploration:  mean-value profile by box-and-whisker plots
# ------------------------------------------------------------------------------

myPanel <- function(x, y, subscripts, ...){
  panel.grid(h = -1, v = 0)    
  panel.bwplot(x, y, ...) 
}


bw1 <- bwplot(visual ~ time.f | treat.f,
              panel = myPanel,
              data = armd0)


xlims <- c("Base", "4\nwks", "12\nwks", "24\nwks", "52\nwks")


bw1a <- update(bw1, xlim = xlims, pch = "|")


print(bw1a)



# -->
# The box-and-whiskers plots illustrate the patterns implied by the sample means and medians.
# The decrease of the mean values in time is clearly seen for both treatment groups.

# It is more pronounced for the active-treatment arm.
# As there was a slightly higher dropout in that arm, a possible explanation could be that patients
# whose visual acuity improved dropped out of the study.

# In such case, a faster progression of the disease in that treatment arm would be observed.



# ------------------------------------------------------------------------------
# Data exploration:  mean-value profile for monotone missing-data patterns
# ------------------------------------------------------------------------------

xtabs(~ treat.f + miss.pat, data = armd.wide)


# only monotone missing-data patterns
armd.wide.mnt <- armd.wide %>% filter(miss.pat %in% c("----", "---X", "--XX", "-XXX", "XXXX"))



# ----------
# drop levels
armd.wide.mnt <- droplevels(armd.wide.mnt)


levels(armd.wide.mnt$miss.pat)



# ----------

attach(armd.wide.mnt)

( dat <- list(visual0, visual4, visual12, visual24, visual52) )

( flst <- list(miss.pat, treat.f) )


tmp <- lapply(1:length(dat), function(x) tapply(dat[[x]], flst, FUN = mean))

names(tmp) <- c("0wks", "4wks", "12wks", "24wks", "52wks")

res <- bind_rows(tmp)



# ----------
nms1 <- c(rep("Active", 5), rep("Placebo", 5))

nms2 <- rep(c("----", "---X", "--XX", "-XXX", "XXXX"), 2)


res$treat.f <- nms1

res$miss.pat <- nms2

res_o <- res



# ----------

res <- reshape::melt(data.frame(res), id.vars = c("treat.f", "miss.pat"))

from <- c("X0wks", "X4wks", "X12wks", "X24wks", "X52wks")

to <- c(0, 4, 12, 24, 52)

res$time <- to[match(res$variable, from)]



# ----------
xy1 <- xyplot(value ~ time | treat.f, 
              groups = miss.pat,
              data = res,
              type = "l", lty = 1)


update(xy1, xlab = "Time (in weeks)", ylab = "Visual acuity", grid = "h", ylim = c(0, 80))


res_o




# -->
# the mean-value profiles for missing-data patterns with a larger numver of missing values,
# are based on measurements for a small number of patients.
# Thus, the variability of these profiles is larger than for the patterns with a smaller number of missing values.


detach(armd.wide.mnt)

