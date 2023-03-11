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
# assess parameter distribution
# ------------------------------------------------------------------------------

MyBUGSOutput <- function(Output  = Output, SelectedVar = SelectedVar, VarNames = NULL){
  xx   <- Output
  vars <- SelectedVar
  
  if (is.null(VarNames)) { VarNames <- SelectedVar }
  if (length(SelectedVar) != length(VarNames)) {stop("Wrong number of variable names")}
  
  x <- xx$sims.matrix
  OUT <- matrix(nrow = length(vars), ncol=4) 
  j<-1
  for(i in vars){
    xi <- x[,i]	
    OUT[j,3:4] <- quantile(xi, probs = c(0.025, 0.975))
    OUT[j,1]   <- mean(xi)
    OUT[j,2]   <- sd(xi)
    j          <- j + 1
  }
  colnames(OUT) <- c("mean", "se", "2.5%", "97.5%")
  rownames(OUT) <- VarNames
  OUT
}


OUT1 <- MyBUGSOutput(K2$BUGSoutput, c(uNames("beta", K)))

print(OUT1, digits =3)



# ----------
MyBUGSHist <- function(Output = Output, SelectedVar = SelectedVar, PanelNames = NULL){
  #Small function to make an histogram of the ACF per chain.
  #xx$BUGSoutput is the out object from JAGS
  #vars is a character string of variables in xx
  #PanelNames are matching names for the panels
  
  #for each variable 
  x <- Output$sims.matrix
  AllParams <- NULL
  
  if (is.null(PanelNames)) { PanelNames <- SelectedVar }
  if (length(SelectedVar) != length(PanelNames)) {stop("Wrong number of panel names")}
  
  
  for (i in SelectedVar){
    #Extract data from variable i
    Paramsi <- x[,i]
    AllParams <- c(AllParams, Paramsi)	
  }
  
  #AllID <- rep(vars, each = nrow(x))
  AllID2 <- rep(PanelNames, each = nrow(x))
  AllID2 <- factor(AllID2, levels = PanelNames)
  
  
  MyPanelCex <- 1.2
  Z <- histogram( ~ AllParams | factor(AllID2),
                  strip = strip.custom(bg = 'white',
                                       par.strip.text = list(cex = MyPanelCex)),
                  type = "count" ,
                  nint = 100,
                  xlab = list(label = "Posterior distribution", cex = 1.5),
                  col = gray(0.5), 
                  ylab = list(label = "Frequencies", cex = 1.5),
                  scales = list(alternating = FALSE, 
                                x = list(relation = "free"),
                                y = list(relation = "free")),
                  breaks=NULL,              
                  panel = function(x, ...) {
                    panel.histogram(x, ...)
                    panel.abline(v = 0, lwd = 3, col =2)
                    CI <- quantile(x, probs = c(0.025,0.975))
                    panel.arrows (CI[1],-2, CI[2],-2, col = 2, lwd= 7, length=0)
                  })
  print(Z)
}


# ----------

vars <- c(uNames("beta",4))

MyBUGSHist(K2$BUGSoutput, vars)

