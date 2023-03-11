setwd("//media//kswada//MyFiles//R//squirrels")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Red Squirrels
# ------------------------------------------------------------------------------

SQ <- read.table(file = "RedSquirrels.txt", header = TRUE, dec = ".")


str(SQ)


# ----------
# remove large DBH
SQ2 <- subset(SQ, DBH < 0.6)



# ------------------------------------------------------------------------------
# Scaling covariate  (to compare with MCMC result)
# ------------------------------------------------------------------------------

SQ2$Ntrees.std      <- as.numeric(scale(SQ2$Ntrees))

SQ2$TreeHeight.std  <- as.numeric(scale(SQ2$TreeHeight))

SQ2$CanopyCover.std <- as.numeric(scale(SQ2$CanopyCover))



# ------------------------------------------------------------------------------
# Fitting Poisson GLM model in JAGS
# assess ACF per chain
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


vars <- c(uNames("beta",4))

MyBUGSACF(K2$BUGSoutput, vars)



# -->
# There is minimal auto-correlation in the chains.
# If auto-correlation were present, we could ignore it, increase the thinning rate, simplify the model,
# or use a different method of sampling new iterations in teh MCMC process.
