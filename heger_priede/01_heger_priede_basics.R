setwd("//media//kswada//MyFiles//R//heger_priede")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Heger Priede
#   - data from Heger et al. (2008) who looked at deep-sea pelagic bioluminescence (light omission of a living organism) in the summer of 2004
#     during a voyage of the research vessel G.O.Sars from the Azores to Iceland.
#   - A camera mounted on a so-called lander, connected to the boat with a winch cable (Priede et al. 2006), was lowered through the water column.
#     As the system descended, any potentially bioluminescent organisms striking against, or passing through, a mesh screeen were mecahnically stimulated,
#     and light output was recorded. Depth, temperature, salinity, oxygen, and fluorescence levels were measured.
# ------------------------------------------------------------------------------
BL <- read.table(file = "//media//kswada//MyFiles//references//ZuurBeginnersGuideToGeneralizedAdditiveModelsWithR//HegerPriede.txt", header = TRUE)


str(BL)

names(BL)


# no missing value
colSums(is.na(BL))



# ------------------------------------------------------------------------------
# relationship between Sources and Depth
# ------------------------------------------------------------------------------

# The data were sampled at 14 tstations, but in the beginning our analysis we will ignore any potential station effects.

# scatterplot of depth in metres versus bioluminescence counts per cubic meter (called "Sources" in the data file)

# Scale Depth
BL$DepthOriginal <- BL$Depth
BL$Depth <- BL$Depth/max(BL$Depth)

par(mfrow=c(1,2))
plot(x = BL$DepthOriginal, y = BL$Sources, xlab = "Depth", ylab ="Sources",  cex.lab = 1.5)
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))



# ------------------------------------------------------------------------------
# Assess collineartity among continuous variables:  multi-panel scatterplots
# ------------------------------------------------------------------------------

Mypairs <- function(Z) {
  MyVarx <- colnames(Z)
  pairs(Z, labels = MyVarx,
        cex.labels =  2,
        lower.panel = function(x, y, digits=2, prefix="", cex.cor = 7) {
          panel.cor(x, y, digits, prefix, cex.cor)}, 
        upper.panel =  function(x, y) points(x, y, 
                                             pch = 16, cex = 0.8, 
                                             col = gray(0.1)))
  #print(P)
}


panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1 = cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}

MyVar <- c("Sources", "Depth", "Temp", "Salinity", "Oxgen", "flcugl")

Mypairs(BL[, MyVar])



# ------------------------------------------------------------------------------
# Assess collineartity between continuous covariates and categorical covariates:  multi-panel boxplots
# ------------------------------------------------------------------------------

Mybwplot <- function(Z, MyVar, TargetVar){
  #Multipanel boxplots
  #Z: data set
  #MyVar: character string
  #TargetVar: variable for the x-axis..must be a factor
  
  AllY <- as.vector(as.matrix(Z[,MyVar]))
  AllX <- rep(Z[,TargetVar], length(MyVar))
  ID <- rep(MyVar, each = nrow(Z))
  
  P <- bwplot(AllY ~ factor(AllX) | ID, horizontal = FALSE,
              ylab = "", xlab = "",
              scales = list(alternating = T,cex.lab = 1.5,
                            x = list(relation = "same",rot =90, abbreviate = TRUE, cex = 1.5),
                            y = list(relation = "free", draw = FALSE)),
              strip = strip.custom(bg = 'white',
                                   par.strip.text = list(cex = 1.2)),
              cex = .5,
              par.settings = list(
                box.rectangle = list(col = 1),
                box.umbrella  = list(col = 1),
                plot.symbol   = list(cex = .5, col = 1)))
  print(P)
}


MyVar <- c("Sources", "Depth", "Temp", "Salinity", "Oxgen", "flcugl")

Mybwplot(BL, MyVar, "Station")

