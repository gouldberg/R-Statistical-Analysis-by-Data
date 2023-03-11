setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# model inspection
# ------------------------------------------------------------------------------

post <- extract.samples(mod.centered)

a <- apply( post$a_individual , 2 , mean )

b_age <- apply( post$b_age_individual , 2 , mean )

plot(a, b_age, xlab="intercept per subject" , ylab="slope for age per subject", pch=16 , col=rangi2, xlim=c(-2.2,1), ylim=c(-2.2,1))

abline(a=0, b=1, lty=2)


# Roughly half of the observations lie above line x=y and another part - below. 
# From this fact it's hard to say which of varying parts (per intercept or slope) has higher influence on the height estimates.
# Deviance of the a_individual is bigger (sigma_ind[1]) than deviance of the slope b_age_individual(sigma_ind[2]). 
# I assume, that it can be interpreted as a variance of height across individuals is bigger than a variance of growth speed.


