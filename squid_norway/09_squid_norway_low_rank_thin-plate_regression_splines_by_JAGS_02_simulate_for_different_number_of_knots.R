setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ----------
# for comparison with MCMC approach later, we standardize the covariate
Mystd <- function(x) {(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}

Squid$Lat.std <- Mystd(Squid$Lat)
Squid$ML.std  <- Mystd(Squid$ML)



# ------------------------------------------------------------------------------
# Low rank thin-plate regression splines in JAGS:  Simulate for different number of knots  -->  IT TAKES TIME !!!!
# ------------------------------------------------------------------------------
fm  <- NULL
fl  <- NULL
fu  <- NULL
fid <- NULL

for (k in 2:15) {
  K <- k
  Knots <- default.knots(Squid$ML.std, K)
  Z <- GetZ_LRTP(Squid$ML.std, Knots)
  
  win.data2 <- list(Y    = Squid$d15N,     #Response variable
                    Xcov = Xcov,           #Covariates  
                    N    = nrow(Squid),    #Sample size
                    M    = ncol(Xcov),     #Betas 
                    Xspl = X,              #Basis for smoother 
                    Zspl = Z,
                    Mz   = ncol(Z)         #Number of us for smoother basis
  )
  
  K1   <- jags(data       = win.data2,
               inits      = inits2,
               parameters = params2,
               model      = "GAMlrtprs.txt",
               n.thin     = 10,
               n.chains   = 3,
               n.burnin   = 14000,
               n.iter     = 15000)
  
  K2 <- update(K1, n.iter = 20000) 
  out <- K2$BUGSoutput
  
  b <- out$sims.list$b  
  u <- out$sims.list$u
  ML.100 <- seq( -1.4,  3, length = 100)
  Z100 <- GetZ_LRTP(ML.100, Knots)
  f1 <-  ML.100 %*% t(b) + Z100 %*% t(u) 
  f1.info <- MySmoother(f1)
  
  fm <- c(fm,f1.info[,4])
  fl <- c(fl,f1.info[,1])
  fu <- c(fu,f1.info[,3])
  fid<- c(fid,rep(k, 100))
  print(k)
}



fx <- rep(ML.100, 14)


xyplot(fm ~ fx | factor(fid),
       xlab = list(label = "Standardized ML", cex = 1.5),
       ylab = list(label = "Smoother", cex = 1.5),
       ylim = c(-2,2),
       panel = function(x, y, subscripts) {
         panel.lines(x, y, lwd = 2, col = 1)
         panel.lines(x, fl[subscripts], lty = 2, lwd = 2, col = 1)
         panel.lines(x, fu[subscripts], lty = 2, lwd = 2, col = 1)
       })



# -->
# It indeed seems to be the case that the number of knots has minimal effect on the shape of the smoother,
# provided the chosen number of knots in large enough.

