# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\cheddar")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cheddar
# ------------------------------------------------------------------------------

dat <- read.csv("cheddar.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# Least Trimmed Squares
#   - minimize the sum of squares of the q smallest residuals
#   - This method has a high breakdown point because it can tolerate a large number of outliers depending on how q is chosen.
#   - default choice of q = [n/2] + [(p+1)/2] where [x] indicates the largest integer less than or equal to x.
# ------------------------------------------------------------------------------

set.seed(123)

ltsmod <- ltsreg(taste ~ H2S + Lactic, data = dat)


ltsmod



# ----------
coef(mod1)

coef(rlmod)

coef(ltsmod)




# ------------------------------------------------------------------------------
# Least Trimmed Squares:  bootstrapping
# ------------------------------------------------------------------------------


# nsamp = "best":  in order to avoid long computing time, we use "best", 
# but bootstrap estimates of variability will be somewhat on the high side

bcoef <- matrix(0, 1000, 3)

for(i in 1:1000){
  
  newy <- predict(ltsmod) + residuals(ltsmod)[sample(nrow(dat), rep = T)]
  
  brg <- ltsreg(newy ~ H2S + Lactic, data = dat, nsamp = "best")
  
  bcoef[i,] <- brg$coef
}


bcoef <- data.frame(bcoef)

colnames(bcoef) <- names(coef(ltsmod))


( tmp <- apply(bcoef, 2, function(x) quantile(x, c(0.025, 0.975))) )



# ----------

library(ggplot2)


# ggplot(bcoef, aes(x = Acetic)) + geom_density() + geom_vline(xintercept = tmp[,"Acetic"], lty = 2) + theme_bw()


ggplot(bcoef, aes(x = H2S)) + geom_density() + geom_vline(xintercept = tmp[,"H2S"], lty = 2) + theme_bw()


ggplot(bcoef, aes(x = Lactic)) + geom_density() + geom_vline(xintercept = tmp[,"Lactic"], lty = 2) + theme_bw()




# -->
# bootstrap estimation for Lactic coeff might be zero ...
