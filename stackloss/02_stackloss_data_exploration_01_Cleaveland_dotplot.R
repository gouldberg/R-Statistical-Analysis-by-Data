# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\stackloss")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  stackloss
# ------------------------------------------------------------------------------

dat <- read.csv("stackloss.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# Data exploration:  multi-panel Cleveland dotplot
# ------------------------------------------------------------------------------


library(lattice)


Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)), groups=FALSE,
               strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE), y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data from text file", cex = 1.5))
  
  print(P)  
}




# ----------
graphics.off()


MyVar1 <- colnames(dat)


Mydotplot(as.matrix(dat[,MyVar1]))

