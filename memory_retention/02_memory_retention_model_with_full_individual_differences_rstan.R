setwd("//media//kswada//MyFiles//R//memory_retention//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  Recognition Memory
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------
t     <- c(1, 2, 4, 7, 12, 21, 35, 59, 99, 200)
nt    <- length(t)
slist <- 1:4
ns    <- length(slist)

k1 <- matrix(c(18, 18, 16, 13, 9, 6, 4, 4, 4, NA,
               17, 13,  9,  6, 4, 4, 4, 4, 4, NA,
               14, 10,  6,  4, 4, 4, 4, 4, 4, NA,
               NA, NA, NA, NA,NA,NA,NA,NA,NA, NA), nrow=ns, ncol=nt, byrow=T)


# excluding NAs for Stan solution
k <- k1[1:(ns - 1), 1:(nt - 1)]



# to be passed on to Stan
n <- 18
data <- list(k=k, n=n, t=t, ns=ns, nt=nt)

myinits <- list(list(alpha=.5, beta=.1))



# ------------------------------------------------------------------------------
# model
# ------------------------------------------------------------------------------
#### Notes to Stan model #######################################################
## 1) This model doesn't work well in the case of first subject - its alpha and
##    and beta estimates are wrong. For more about this problem search
##    "Memory retention model" in Stan mailing list.
## 2) Missing values are handled by restricting data matrix to k[ns-1,nt-1]  
################################################################################

model <- "
// Retention With Full Individual Differences
data { 
  int ns;
  int nt;
  int k[ns - 1,nt - 1];  // excluding missing values
  int t[nt];
  int n;
}

parameters {
  vector<lower=0,upper=1>[ns] alpha;
  vector<lower=0,upper=1>[ns] beta;
} 

transformed parameters {
  matrix<lower=0,upper=1>[ns,nt] theta;
  
  // Retention Rate At Each Lag For Each Subject Decays Exponentially
  for (i in 1:ns)
    for (j in 1:nt)
      theta[i,j] = fmin(1.0, exp(-alpha[i] * t[j]) + beta[i]);
}

model {
  // Priors
  alpha ~ beta(1, 1);  // can be removed
  beta ~ beta(1, 1);  // can be removed
  
  // Observed Data
  for (i in 1:(ns - 1))
    for (j in 1:(nt - 1))
      k[i,j] ~ binomial(n, theta[i,j]);
}

generated quantities {
  int<lower=0,upper=n> predk[ns,nt];
  
  // Predicted Data
  for (i in 1:ns)
    for (j in 1:nt)
      predk[i,j] = binomial_rng(n, theta[i,j]);
}"


# ------------------------------------------------------------------------------
# parameter estimation
# ------------------------------------------------------------------------------
parameters <- c("alpha", "beta", "predk")

samples <- stan(model_code=model,   
                data=data, 
                init=myinits,  # If not specified, gives random inits
                pars=parameters,
                iter=20000, 
                chains=1, 
                thin=1
                # warmup = 100,  # Stands for burn-in; Default = iter/2
                # seed = 123  # Setting seed; Default is random seed
)



# ------------------------------------------------------------------------------
# Check traceplot
# ------------------------------------------------------------------------------
traceplot(samples)

print(samples)



# ------------------------------------------------------------------------------
# model inspection:  joint distribution of alpha and beta
# ------------------------------------------------------------------------------
n.iter <- 10000
keepi <- 500
keep <- sample(n.iter, keepi)

alpha1 <- extract(samples)$alpha[, 1]
alpha2 <- extract(samples)$alpha[, 2]
alpha3 <- extract(samples)$alpha[, 3]
alpha4 <- extract(samples)$alpha[, 4]

beta1 <- extract(samples)$beta[, 1]
beta2 <- extract(samples)$beta[, 2]
beta3 <- extract(samples)$beta[, 3]
beta4 <- extract(samples)$beta[, 4]
d.beta1 <- density(beta1)
d.beta2 <- density(beta2)
d.beta3 <- density(beta3)
d.beta4 <- density(beta4)



# ----------
layout(matrix(c(1,2,3,0),2,2,byrow=T), width=c(2/3, 1/3), heights=c(2/3,1/3))
#layout.show()

par(mar=c(2,2,1,0))
plot(alpha1[keep],beta1[keep], xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), axes=F)
points(alpha2[keep],beta2[keep], col="red")
points(alpha3[keep],beta3[keep], col="green")
points(alpha4[keep],beta4[keep], col="blue")
box(lty=1)

par(mar=c(2,1,1,4))
plot(d.beta1$y, d.beta1$x, ylim=range(c(0,1)), xlim=c(12,0),type='l', axes=F, xlab="", ylab="")
#plot(d.beta1$y, d.beta1$x, ylim=range(c(0,1)), xlim=rev(range(d.beta1$y)),
#     type='l', axes=F, xlab="", ylab="")
lines(d.beta2$y, d.beta2$x, col="red")
lines(d.beta3$y, d.beta3$x, col="green")
lines(d.beta4$y, d.beta4$x, col="blue")
axis(4, at=c(0,1))
mtext(expression(beta), side=4,line=1, cex=1.3)
box(lty=1)

par(mar=c(6,2,0,0))
plot(density(alpha1),zero.line=F ,main="", ylab="", xlab="", cex.lab=1.3, xlim=c(0,1), axes=F)
lines(density(alpha2), col="red")
lines(density(alpha3), col="green")
lines(density(alpha4),col="blue")
axis(1,at=c(0,1))
mtext(expression(alpha), side=1.2,line=1, cex=1.3)
box(lty=1)



# ------------------------------------------------------------------------------
# model inspection:  prediction of time * memory retention by individual
# ------------------------------------------------------------------------------
layout(matrix(c(1:4),2,2,byrow=T))
#layout.show()

sc <- 3.5
jj <- numeric()
xx <- numeric()

for (i in 1:ns) {
  plot(-1,100,xlim=c(0,10),ylim=c(0,18), main=(paste("Subject", i)),
       xlab=("Time Lags"), ylab=("Retention Count"),cex.lab=1.3, axes=F)
  axis(1, at=c(1,2,3,4,5,6,7,8,9,10), cex.axis=0.7,
       lab=c("1","2","4","7","12","21","35","59","99","200"))
  axis(2, at=c(0,18),lab=c("0","18"),cex.axis=0.7)
  box(lty=1)
  for (j in 1:nt) {
    count <- hist(extract(samples)$predk[,i,j],c(0:n),plot=F)
    count <- count$counts
    count <- count/sum(count)
    for (x in 1:n){
      if (count[x]>0){
        points(j,x,pch=22, col="black",cex=sc*sqrt(count[x]))
        if (!is.na(k1[i,j]) && k1[i,j]==x){
          points(j,x,pch=22,bg="black",cex=sc*sqrt(count[x]))
          jj <- c(jj,j)
          xx <- c(xx,x)
        }
      }
    }
  }
  coords <- list(x=jj, y=xx)
  lines(coords,lwd=2)
  jj <- numeric()
  xx <- numeric()
}


# -->
# For person #4, only prior distribution is applied
