## Empirical Bayesian
## ----------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\Empirical_Bayesian")

theme_set(theme_bw())


# -----------------------------------------------------------------------------
# GLM gaussian
# -----------------------------------------------------------------------------
library(knitr)
opts_chunk$set(cache = TRUE, warning = FALSE, tidy = FALSE, fig.height=5, fig.width=3, dev="cairi_pdf")
library(ggplot2)
theme_set(theme_bw())
options(tibble.print_min = 6, scipen = 7)
library(dplyr)

data <- read.delim("clipboard", header=T, stringsAsFactors=F)

ggplot(data, aes(x = hipcenter, y = ..density..)) + geom_histogram(binwidth=10) + geom_density(color="blue")

mod <- glm(data=data, formula = hipcenter ~., family=gaussian)
summary(mod)

confint(mod)
fitted(mod)

# prediction inverval
pr <- predict(mod, newdata=data, se.fit=TRUE)
family <- family(mod)
lower <- family$linkinv(pr$fit - qnorm(0.95) * pr$se.fit)
upper <- family$linkinv(pr$fit + qnorm(0.95) * pr$se.fit)



# The standard non-informative prior for the linear regression analysis example (Bayesian Data Analysis 2nd Ed, p:355-358)
# takes an improper (uniform) prior on the coefficients of the regression
# and the logarithm of the residual variance \sigma^2. With these priors, the posterior distribution of \beta conditional on \sigma^2 and
# the response variable y is: \beta|\sigma^2 , y \sim N(\hat{\beta},V_{\beta}\sigma^2)

# The marginal posterior distribution for \sigma^2 is a scaled inverse \chi^2 distribution with scale s and n-k degrees of freedom,
# where n is the number of data points and k the number of predictor variables. In our example these assume the values of n=20, k=2,
# while s^2 is the standard frequentist estimate of the residual variance.
# The quantities \hat{\beta}, s^2 are directly available from the information returned by R’s lm, while V_{\beta} can be computed from the qr element of the lm object:

glmfit <- glm(data=data, formula = hipcenter ~., family=gaussian)


QR<-glmfit$qr
df.residual<-glmfit$df.residual
R<-qr.R(QR) ## R component
coef<-glmfit$coef
Vb<-chol2inv(R) ## variance(unscaled)
s2<-(t(glmfit$residuals)%*%glmfit$residuals)
s2<-s2[1,1]/df.residual

## function to compute the bayesian analog of the lmfit
## using non-informative priors and Monte Carlo scheme
## based on N samples


bayesfit<-function(glmfit,N){
    QR<-glmfit$qr
    df.residual<-glmfit$df.residual
    R<-qr.R(QR) ## R component
    coef<-glmfit$coef
    Vb<-chol2inv(R) ## variance(unscaled)
    s2<-(t(glmfit$residuals)%*%glmfit$residuals)
    s2<-s2[1,1]/df.residual

    ## now to sample residual variance
    sigma<-df.residual*s2/rchisq(N,df.residual)
    coef.sim<-sapply(sigma,function(x) mvrnorm(1,coef,Vb*x))
    ret<-data.frame(t(coef.sim))
    names(ret)<-names(glmfit$coef)
    ret$sigma<-sqrt(sigma)
    ret
}

Bayes.sum<-function(x)
    {
        c("mean"=mean(x),
          "se"=sd(x),
          "t"=mean(x)/sd(x),
          "median"=median(x),
          "CrI"=quantile(x,prob=0.025),
          "CrI"=quantile(x,prob=0.975)
          )
    }

library(MASS)
set.seed(1234)  ## reproducible sim
glmfit <- glm(data=data, formula = hipcenter ~., family=gaussian)
bf<-bayesfit(glmfit,10000)
t(apply(bf,2,Bayes.sum))




# ----
# Residuals Bayesian Updating
set.seed(123)
n <- 1000
m <- 1.4
sd <- 2.7
x     <- rnorm(n, mean=m, sd=sd)

mu    <- numeric(n)
sigma <- numeric(n)
sim_n <- 10000

( mu[1]    <- (sim_n*x[1] + (sd^2)*0)/(sim_n+sd^2) )
( sigma[1] <- (sim_n*sd^2)/(sim_n+sd^2) )

# bayesian updating for gaussian
# Since normal distribution is a conjugate prior for μ
 of normal distribution, we have closed-form solution to update the prior
for (i in 2:n) {
  mu[i]    <- ( sigma[i-1]*x[i] + (sd^2)*mu[i-1] )/(sigma[i-1]+sd^2)
  sigma[i] <- ( sigma[i-1]*sd^2                  )/(sigma[i-1]+sd^2)
}

dat <- data.frame(mu = mu, sigma = sigma)

ts.plot(dat["mu"] , type="l")
ts.plot(dat["sigma"] , type="l")

i <- 1
crossing(x = seq(-6, 6, 0.1)) %>%
  mutate(density = dnorm(x, mean=dat[i,"mu"], sd=sqrt(dat[i,"sigma"]))) %>%
  ggplot(aes(x, density)) + geom_line()

i <- 10
crossing(x = seq(-6, 6, 0.1)) %>%
  mutate(density = dnorm(x, mean=dat[i,"mu"], sd=sqrt(dat[i,"sigma"]))) %>%
  ggplot(aes(x, density)) + geom_line()


i <- 25
crossing(x = seq(-6, 6, 0.1)) %>%
  mutate(density = dnorm(x, mean=dat[i,"mu"], sd=sqrt(dat[i,"sigma"]))) %>%
  ggplot(aes(x, density)) + geom_line()
