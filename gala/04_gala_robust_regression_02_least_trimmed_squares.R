]setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)




# ------------------------------------------------------------------------------
# models
# ------------------------------------------------------------------------------

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)

lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, data = gala)

lmods2 <- lm(Species ~ Elevation + Nearest + Scruz, data = gala)

lmod3 <- lm(Species ~ I(1 * Area + 1 * Adjacent) + Elevation + Nearest + Scruz, data = gala)

lmod4 <- lm(Species ~ Area + offset(0.5 * Elevation) + Nearest + Scruz + Adjacent, data = gala)

lmod_final <- lm(Species ~ Elevation + Adjacent, data = gala)

lmod_r_final <- lm(Species ~ Area + Elevation + Adjacent, data = gala[-16,])



# ----------
# not excluding Isabela
lmod_r2_final <- lm(Species ~ Area + Elevation + Adjacent, data = gala)





# ------------------------------------------------------------------------------
# Least Trimmed Squares
#   - minimize the sum of squares of the q smallest residuals
#   - This method has a high breakdown point because it can tolerate a large number of outliers depending on how q is chosen.
#   - default choice of q = [n/2] + [(p+1)/2] where [x] indicates the largest integer less than or equal to x.
# ------------------------------------------------------------------------------

set.seed(123)

ltsmod <- ltsreg(Species ~ Area + Elevation + Nearest+ Scruz + Adjacent, data = gala)


ltsmod



# ----------
coef(lmod)

coef(rlmod)

coef(ltsmod)




# ------------------------------------------------------------------------------
# Least Trimmed Squares:  exhaustive search
# ------------------------------------------------------------------------------

ltsmod_e <- ltsreg(Species ~ Area + Elevation + Nearest+ Scruz + Adjacent, data = gala, nsamp = "exact")


coef(ltsmod)

coef(ltsmod_e)

coef(lmod)




# -->
# different coefficients
# Area has larger coefficients and Elevation has smaller coefficients compared OLS




# ------------------------------------------------------------------------------
# Least Trimmed Squares:  bootstrapping
# ------------------------------------------------------------------------------


# nsamp = "best":  in order to avoid long computing time, we use "best", 
# but bootstrap estimates of variability will be somewhat on the high side

bcoef <- matrix(0, 1000, 6)

for(i in 1:1000){
  
  newy <- predict(ltsmod) + residuals(ltsmod)[sample(nrow(gala), rep = T)]
  
  brg <- ltsreg(newy ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala, nsamp = "best")
  
  bcoef[i,] <- brg$coef
}


bcoef <- data.frame(bcoef)

colnames(bcoef) <- names(coef(ltsmod))


( tmp <- apply(bcoef, 2, function(x) quantile(x, c(0.025, 0.975))) )



# ----------

library(ggplot2)


ggplot(bcoef, aes(x = Area)) + geom_density() + geom_vline(xintercept = tmp[,"Area"], lty = 2) + theme_bw()


ggplot(bcoef, aes(x = Adjacent)) + geom_density() + geom_vline(xintercept = tmp[,"Adjacent"], lty = 2) + theme_bw()




# -->
# the distribution is more peaked than a normal distribution with some long tails.

# The conclusion is that the area variable is significant.
# That is in contrast to the conclusion from the least squares fit.

