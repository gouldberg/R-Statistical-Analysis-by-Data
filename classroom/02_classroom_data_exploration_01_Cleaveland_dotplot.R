
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\classroom")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SII Project, classroom
# ------------------------------------------------------------------------------


data(classroom, package = "WWGbook")


str(classroom)


car::some(classroom)




# ----------
# convert to factor variable

SIIdata <- within(classroom,
                  {
                    sex <- factor(sex, levels = c(0,1), labels = c("M", "F"))
                    minority <- factor(minority, labels = c("Mnrt:No", "Mnrt:Yes"))
                    schoolid <- factor(schoolid)
                    classid <- factor(classid)
                    childid <- factor(childid)
                  })


str(SIIdata)



# ------------------------------------------------------------------------------
# Data exploration:  multi-panel Cleveland dotplot
# ------------------------------------------------------------------------------


library(lattice)


Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)), groups = FALSE,
               strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
#               scales = list(x = list(relation = "free", draw = TRUE), y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data from text file", cex = 1.5))
  
  print(P)  
}




# ----------
graphics.off()


MyVar1 <- colnames(SIIdata)


Mydotplot(SIIdata[,MyVar1])




# ----------

car::densityPlot(SIIdata$ses)


car::densityPlot(SIIdata$mathgain)

