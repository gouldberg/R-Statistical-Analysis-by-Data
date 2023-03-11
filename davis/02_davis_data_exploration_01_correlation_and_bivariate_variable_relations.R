rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\regression_basics")


# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------

data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)




# ------------------------------------------------------------------------------
# Data exploration:  data distribution
# ------------------------------------------------------------------------------


summary(data)



# ----------
psych::describe(data)




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


MyVar <- c("height", "repwt", "repht", "weight")

graphics.off()
Mydotplot(data[,MyVar])




# ------------------------------------------------------------------------------
# Data exploration:  pairs plot
# ------------------------------------------------------------------------------

# na.exclude() is required


psych::pairs.panels(na.exclude(data[,MyVar]), stars = TRUE)

# psych::pairs.panels(data[,MyVar], stars = TRUE)


psych::pairs.panels(na.exclude(data[,MyVar]), method = "spearman", stars = TRUE)

# psych::pairs.panels(data[,MyVar], method = "spearman", stars = TRUE)



# -->
# Note that there are some outliers



# ------------------------------------------------------------------------------
# Data exploration:  corrplot
# ------------------------------------------------------------------------------

library(corrplot)


cor_mat <- cor(data %>% dplyr::select(-sex) %>% na.exclude(), method = "pearson")


corrplot(cor_mat, hclust.method = "ward.D2", addrect = TRUE)




# ------------------------------------------------------------------------------
# Data exploration:  paris plot by scatterplot matrix
# ------------------------------------------------------------------------------


library(car)


formula <- ~ height + repwt + repht + weight


scatterplotMatrix(formula, data = data,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)





# ------------------------------------------------------------------------------
# Data exploration:  paris plot by scatterplot matrix by group
# ------------------------------------------------------------------------------

# by group

formula <- ~ height + repwt + repht + weight | sex

scatterplotMatrix(formula, data = data,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black"), pch = 20)




# ------------------------------------------------------------------------------
# Data exploration:  distribution by group
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

boxplot(weight ~ sex, data = data)

boxplot(repwt ~ sex, data = data)

boxplot(repht ~ sex, data = data)

boxplot(height ~ sex, data = data)


