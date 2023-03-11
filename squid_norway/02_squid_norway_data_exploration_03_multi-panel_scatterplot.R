setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ------------------------------------------------------------------------------
# Data exploration:  Multipanel scatterplot
# ------------------------------------------------------------------------------
Myxyplot <- function(Z, MyV, NameY1, MyXlab = "", MyYlab="") {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  
  library(mgcv)
  library(lattice)
  
  P <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(MyXlab, cex = 1.5),
              #ylab = list("Response variable", cex = 1.5),
              #ylab = list("Pearson residuals", cex = 1.5),
              ylab = list(MyYlab, cex = 1.5),
              #layout = c(2,2),   #Modify
              strip = function(bg='white', ...) strip.default(bg='white', ...),
              scales = list(alternating = T, x = list(relation = "free"), y = list(relation = "same")),
              panel=function(x, y){
                panel.grid(h=-1, v= 2)
                panel.points(x, y, col = 1)
                panel.loess(x, y, span = 0.8,col = 1, lwd = 2)
              })
  
  print(P)
}


MyVar <- c("Lat", "Depth", "ML")


Myxyplot(Squid, MyVar, "d15N")



# -->
# Depth and latitude do not show a noticeable relationship to d15N, but there is clearly a relationship between mantle length (ML) and d15N.
# Based on the right-most panel, 2 questions arise:
#  - Is the relationship between d15N and mantle length linear or is it non-linear ?
#  - Does the heterogeneity increase for larger mantle length values ?


# ----------
Y   <- Squid$d15N
MyX <- c("Lat", "Depth", "ML")
X   <- Squid[, MyX]

AllX  <- as.vector(as.matrix(Squid[,MyX]))
AllY  <- rep(Squid[,"d15N"] , length(MyX))
AllID <- rep(MyX, each = nrow(Squid))

xyplot(AllY ~ AllX|factor(AllID), 
       col = 1,
       layout = c(3,1),
       xlab = list(label="Explanatory variables",cex = 1.5),
       ylab = list(label = "d15N", cex = 1.5),
       strip = function(bg='white', ...) strip.default(bg='white', ...),
       scales = list(alternating = T, x = list(relation = "free"), y = list(relation = "same")),
       panel=function(x, y){
         panel.grid(h=-1, v= 2)
         panel.points(x, y, col = 1)
         panel.loess(x, y, col = 1, lwd = 2)})

