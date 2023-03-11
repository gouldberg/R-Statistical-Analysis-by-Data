setwd("//media//kswada//MyFiles//R//cafes")

packages <- c("dplyr", "rethinking", "MASS", "ellipse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# inspect posterior distribution
# extract mean posterior slope and intercepts
# ------------------------------------------------------------------------------

extract_mean_posterior_slope_intercept <- function(model){
  post <- extract.samples(model)
  a <- apply( post$a_cafe , 2 , mean )
  b <- apply( post$b_cafe , 2 , mean )
  list(a=a, b=b)
}


post_1 <- extract_mean_posterior_slope_intercept(mod.rho7)

post_2 <- extract_mean_posterior_slope_intercept(mod2.rho7)



# ------------------------------------------------------------------------------
# plot mean intercept(a) and slope(b) estimated by MVN model
# ------------------------------------------------------------------------------

plot( post_1$a , post_1$b,  xlab="intercept" , ylab="slope" ,
      pch=16 , col=rangi2, 
      xlim=c(0,6), ylim=c(-2.5,0)
)


points(post_2$a , post_2$b,  pch=16 , col='black') #estimated by pooled model with no correlation

for ( i in 1:length(post_1$a) ) lines( c(post_1$a[i], post_2$a[i]) , c(post_1$b[i], post_2$b[i]) )



# ----------
# plot contours of joint posterior distribution of slope and intercept
post <- extract.samples(mod.rho7)

Mu_est <- c( mean(post$a) , mean(post$b) )

rho_est <- mean( post$Rho[,1,2] )

sa_est <- mean( post$sigma_cafe[,1] )

sb_est <- mean( post$sigma_cafe[,2] )

cov_ab <- sa_est*sb_est*rho_est

Sigma_est <- matrix( c(sa_est^2,cov_ab,cov_ab,sb_est^2) , ncol=2 )



# ----------
# draw contours
for ( l in c(0.1,0.3,0.5,0.8,0.99) ) lines(ellipse(Sigma_est,centre=Mu_est,level=l), col=col.alpha("black",0.2))



# It can be seen from the plot that black points with small intercept(between 1.5 and 2.5) are shifted up 
#  in the direction of bigger slope(-0.5..0) and, on the contrary, points with large intercept(between 5 and 6) 
#  are shifted towards smaller slope (-1.5).
# This is the effect of shrinkage to the mean in the model that takes into account correlation among slope and intercept.
