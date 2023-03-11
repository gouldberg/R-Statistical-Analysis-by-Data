setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)



# ------------------------------------------------------------------------------
# basic analysis:  Principal Component Analysis
# ------------------------------------------------------------------------------
save <- princomp(formula = ~ density + class.new + hardness + size + weight + moisture, data = wheat2,  cor = TRUE, scores = TRUE)

summary(save, loadings = TRUE, cutoff = 0.0)


# -->
# 3 principal components explaines more than 80% of variance.



par(pty = "s")
# bubble.color<-ifelse(wheat2$class.new == 1, yes = "black", no = "blue")  # Can also see separation of SRW and HRW kernels - use instead of wheat.colors
wheat.colors <- ifelse(test = wheat$type=="Healthy", yes = "black", no = ifelse(test = wheat$type=="Sprout", yes = "red", no = "green"))



# ----------
par(pty = "s")
symbols(x = save$scores[,1], y = save$scores[,2], circles = save$scores[,3]-min(save$scores[,3]), 
        inches=0.25, xlab = "Principal component 1", ylab = "Principal component 2", fg = wheat.colors, 
        main = "Bubble plot for first three principal components \n Wheat data")  # Note that circles can not take on a negative value!
abline(h = 0, lty = 1, lwd = 2)  
abline(v = 0, lty = 1, lwd = 2)  
text(x = save$scores[,1], y = save$scores[,2], col = 2, cex = 0.5)  # Put kernel number on plot

legend(locator(1), legend=c("Healthy", "Sprout", "Scab"), pch = c(1,1,1), 
       col=c("black", "red", "green"), cex=1, bty="n") 



