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
# Linear spline regression in JAGS:  assess model, ACF per chain
# ------------------------------------------------------------------------------
MyBUGSACF <- function(Output  = Output, SelectedVar = SelectedVar, PanelNames = NULL){
  #Small function to make an xyplot of the ACF per chain,
  #for each variable 
  #Output  = is the out object from JAGS
  #SelectedVar is a character string of variables in xx
  #PanelNames are matching names for the panels
  
  if (is.null(PanelNames)) { PanelNames <- SelectedVar }
  if (length(SelectedVar) != length(PanelNames)) {stop("Wrong number of panel names")}
  xx <- Output
  
  x <- xx$sims.array
  idchain.All <- NULL
  acf.Var.All <- NULL
  Lag.Var.All <- NULL
  id.All <- NULL
  
  NumBerChains <- ncol(x[,, SelectedVar[1]])
  
  for (i in SelectedVar){
    
    #Extract data from variable i and calculate ACF for each chain
    x1       <- x[,,i]
    acf.Vari <- Lag.Vari <- NULL
    for (j in 1:NumBerChains){
      acf.Varij <- acf(x1[,j], plot = FALSE)
      acf.Vari <- c(acf.Vari, acf.Varij$acf)
      Lag.Vari <- c(Lag.Vari, acf.Varij$lag)
    }
    NumLags <- length(acf.Varij$lag)
    
    ii <- PanelNames[SelectedVar==i]
    #print(i)
    #print(ii)
    id          <- rep(rep(ii, length = NumLags),NumBerChains)
    idchain     <- rep(1:NumBerChains, each = NumLags)
    
    
    acf.Var.All <- c(acf.Var.All, acf.Vari)
    Lag.Var.All <- c(Lag.Var.All, Lag.Vari)
    id.All <- c(id.All, id)
    idchain.All <- c(idchain.All, idchain)
  }
  
  Z <- xyplot(acf.Var.All ~ Lag.Var.All | factor(id.All) ,
              type = "l",
              strip = strip.custom(bg = 'white',par.strip.text = list(cex = 1.2)),
              scales = list(x = list(relation = "same", draw = TRUE), y = list(relation = "same", draw = TRUE)),
              groups = idchain.All,  col = 1:NumBerChains,
              xlab = list(label = "Lags",cex = 1.5),
              ylab = list(label = "ACF", cex = 1.5))
  print(Z)
}


MyBUGSACF(out, uNames("beta", ncol(X)))
