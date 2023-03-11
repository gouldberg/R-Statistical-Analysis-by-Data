setwd("//media//kswada//MyFiles//R//bangladesh")

packages <- c("dplyr", "rethinking", "lattice", "tidyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bangladesh
# ------------------------------------------------------------------------------
data("bangladesh", package = "rethinking")

d <- bangladesh

dim(d)

str(d)



# ----------
# District 54 is absent
sort(unique(d$district))


d$district_id <- as.integer(as.factor(d$district))
sort(unique(d$district_id))



# ------------------------------------------------------------------------------
# fixed effects model, fitting a different alpha for each district
# ------------------------------------------------------------------------------
mod_fe <- map2stan(
  alist(
    use.contraception ~ dbinom(1, p),
    logit(p) <- alpha[district_id],
    alpha[district_id] ~ dnorm(0, 10)
  ), data=d )


# plot(mod_fe)

precis(mod_fe, digits=3, depth = 2)



# ------------------------------------------------------------------------------
# multilevel model, alpha per district depends on overall distribution of alpha
# ------------------------------------------------------------------------------
mod_ml <- map2stan(
  alist(
    use.contraception ~ dbinom(1, p) ,
    logit(p) <- alpha[district_id],
    alpha[district_id] ~ dnorm(a, sigma),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1)
  ), data=d )



# plot(mod_ml)

precis(mod_ml, digits=3, depth = 2)



# ------------------------------------------------------------------------------
# plot the predicted proportions of women in each district using contraception
# ------------------------------------------------------------------------------
link_fe <- link(mod_fe, data=data.frame(district_id=1:60))
link_fe_mean <- apply(link_fe, 2, mean)
link_fe_PI <- apply(link_fe, 2, PI)

link_ml <- link(mod_ml, data=data.frame(district_id=1:60))
link_ml_mean <- apply(link_ml, 2, mean)
link_ml_PI <- apply(link_ml, 2, PI)


# ----------
d_sum <- d %>% as.tbl %>% group_by(district_id) %>% summarize(p_use_contraception=mean(use.contraception))

plot(y=d_sum$p_use_contraception, x=d_sum$district_id, pch=19, xlab="districts", ylab="proportion using contraception")
abline(h=median(d_sum$p_use_contraception))
legend("topright",legend=c("data","fixed effects", "multilevel"),fill=c("black","red","blue"))

points(y=link_fe_mean, x=1:60, col="red")
for(i in 1:60){segments(x0=i,x1=i,y0=link_fe_PI[1,i],y1=link_fe_PI[2,i],col="red")}

points(y=link_ml_mean,x=(1:60)+0.2, col="blue")
for(i in 1:60){segments(x0=i+0.2,x1=i+0.2,y0=link_ml_PI[1,i],y1=link_ml_PI[2,i],col="blue")}


# The fixed effects model closely follows the data in terms of means (the mean is not accurate when the true proportion is 0 or 1).
# The mixed effects models shrinks all extreme points to the median.


