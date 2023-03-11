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
# Linear spline regression in JAGS:  assess model, chain mixing
# ------------------------------------------------------------------------------
uNames <- function(k,Q){
  #Function to make a string of variables names of the form:
  #c("u[1]","u[2]", etc, "u[50]")	
  #Q=50 knots were used	
  String<-NULL
  for (j in 1:Q){String <- c(String, paste(k,"[",j,"]",sep = ""))}
  String
}


MyBUGSChains <- function(xx, vars, PanelNames = NULL){
  # Small function to make an xyplot of the iterations per chain, for each variable 
  x <- xx$sims.array
  idchain.All <- NULL
  x1.All <- NULL
  ChainLength.All <- NULL
  id.All <- NULL
  
  NumBerChains <- ncol(x[,,vars[1]])
  
  for (i in vars){
    x1          <- as.vector(x[,,i])
    id          <- rep(rep(i, length = nrow(x[,,i])),NumBerChains)
    idchain     <- rep(1:NumBerChains, each = nrow(x[,,i]))
    ChainLength <- rep(1: nrow(x[,,i]), NumBerChains)
    
    x1.All <- c(x1.All, x1)
    ChainLength.All <- c(ChainLength.All, ChainLength)
    id.All <- c(id.All, id)
    idchain.All <- c(idchain.All, idchain)
  }
  
  
  if (!is.null(PanelNames)) { 
    if (length(unique(id.All)) != length(PanelNames)) {stop("Wrong number of panel names")}
    AllNames <- unique(id.All)
    for (i in 1:length(AllNames)) id.All[id.All == AllNames[i]] <- PanelNames[i] 
    id.All <- factor(id.All, levels = PanelNames)
  }
  
  Z <- xyplot(x1.All ~ ChainLength.All | factor(id.All) ,
              type = "l",
              strip = strip.custom(bg = 'white',par.strip.text = list(cex = 1.2)),
              scales = list(x = list(relation = "same", draw = TRUE), y = list(relation = "free", draw = TRUE)),
              groups = idchain.All,  col = 1:NumBerChains,
              xlab = list(label = "MCMC iterations", cex = 1.5),
              ylab = list(label = "Sampled values", cex = 1.5))
  print(Z)
}



# To assess mixing of the six regression parameters and variance parameters, chains are plotted and numerical statistics (Rhat values) can be inspected.
# Mixing of all parameters is good.
out <- K2$BUGSoutput


MyBUGSChains(out, c(uNames("beta", ncol(X)), "sigma"))

