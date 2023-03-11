setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ----------
d$height_normalised <- (d$height - mean(d$height))/sd(d$height)



# ------------------------------------------------------------------------------
# model inspection
# ------------------------------------------------------------------------------

post <- extract.samples(mod.centered2)

a <- apply( post$a_individual, 2 , mean )

b_age <- apply( post$b_age_individual, 2 , mean )



# ----------
plot(a, b_age, xlab="intercept per subject" , ylab="slope for age per subject", pch=16 , col=rangi2, xlim=c(-2.2,1), ylim=c(-2.2,1))

abline(a=0, b=1, lty=2)



# ----------
# contour plot of estimated multivariateive distribution of intercept and slope
Mu_est <- c( mean(post$a_individual) , mean(post$b_age_individual) )

rho_est <- mean( post$Rho[,1,2] )

sa_est <- mean( post$sigma_ind[,1] )

sb_est <- mean( post$sigma_ind[,2] )

cov_ab <- sa_est*sb_est*rho_est

Sigma_est <- matrix( c(sa_est^2,cov_ab,cov_ab,sb_est^2) , ncol=2 )



# draw contours
for ( l in c(0.1,0.3,0.5,0.8,0.99) ) lines(ellipse(Sigma_est, centre=Mu_est, level=l), col=col.alpha("black",0.2))


# Model suggests that 
# intercept and slope are correlated - for big intercepts(boys who are higher on average) the speed of growth are larger(slope for age is bigger)
