# ------------------------------------------------------------------------------
# 可視化:  Parallel Coordinate Plots
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# MASS::parcoord
# ------------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ロジスティック回帰\\")
library(dplyr)
data <- read.csv(".\\AnalysisofCategoricalData\\Chapter3\\wheat.csv", sep=",", header=T)


library(package = MASS)
x11(width = 10, height = 6, pointsize = 12)

data2 <- data.frame(kernel = 1:nrow(data), data[,2:6],  class.new = ifelse(test = data$class == "hrw", yes = 0, no = 1))

data.colors <- ifelse(test = data$type=="Healthy", yes = "black", no = ifelse(test = data$type=="Sprout", yes = "red", no = "green"))
data.lty <- ifelse(test = data$type=="Healthy", yes = "solid", no = ifelse(test = data$type=="Sprout", yes = "longdash", no = "dotdash"))

parcoord(x = data2, col = data.colors, lty = data.lty)
legend(x = 6.15, y = 0.75, legend = c("Healthy", "Sprout", "Scab"), lty = c("solid", "longdash", "dotdash"), col=c("black", "red", "green"), cex=0.8, bty="n")
# legend(locator(1), legend = c("Healthy", "Sprout", "Scab"), lty = c("solid", "longdash", "dotdash"),
# col=c("black", "red", "green"), cex=0.8, bty="n")  #Original legend using locator(1)
# dev.off()  # Create plot for book

data[data$weight == min(data2$weight),]  # 269
order(data$size)  # 268 has the 2nd largest size

# Example highlighting observation #269
data[269,]  # scab
data.colors[269]<-"purple"
line.width <- c(rep(x = 1, times = 268), 10, rep(x = 1, times = 6))
parcoord(x = data2, col = data.colors, lwd = line.width, lty = data.lty, main = "Parallel coordinate plot for data data - highlight kernel 269")
legend(locator(1), legend=c("Healthy", "Sprout", "Scab", "Kernel 269"), lty = c("solid", "longdash", "dotted", "dotted"),
   col=c("black", "red", "green", "purple"), cex=0.75, bty="n", lwd = c(1, 1, 1, 10))

# Sort by data type
data.colors2 <- ifelse(test = data$type=="Healthy", yes = 1, no = ifelse(test = data$type=="Sprout", yes = 2, no = 3))
data3 <- data.frame(data.colors2, data2)
x11(width = 7, height = 7, pointsize = 9)
parcoord(x = data3[order(data.colors2),], col = data.colors[order(data.colors2)],
       main = "Parallel coordinate plot for data data - sort by Type")
legend(locator(1), legend=c("Healthy", "Sprout", "Scab"), lty=c(1,1,1), col=c("black", "red", "green"), cex=1, bty="n")

# Another way to do these plots with brushing (highlight parts of plot to change colors)
library(iplots)
ipcp(data3[order(data.colors2),])


# ------------------------------------------------------------------------------
# ggparallel
# ------------------------------------------------------------------------------
library(ggparallel)

# mtcars
data(mtcars)

ggparallel(list("gear", "cyl"), data=mtcars)
ggparallel(list("gear", "cyl"), data=mtcars, method="hammock", ratio=0.25)

require(RColorBrewer)
require(ggplot2)
cols <- c(brewer.pal(4, "Reds")[-1], brewer.pal(4, "Blues")[-1])
ggparallel(list("gear", "cyl"), ratio=0.2, data=mtcars, method="hammock", text.angle=0) + scale_fill_manual(values=cols) + scale_colour_manual(values=cols) + theme_bw()

# combination of common angle plot and hammock adjustment
ggparallel(list("gear", "cyl"), data=mtcars, method="adj.angle", ratio=2)

# compare with method='parset'
ggparallel(list("gear", "cyl"), data=mtcars, method='parset')

# flip plot and rotate text
ggparallel(list("gear", "cyl"), data=mtcars, text.angle=0) + coord_flip()

# change colour scheme
ggparallel(list("gear", "cyl"), data=mtcars, text.angle=0) + coord_flip() + scale_fill_brewer(palette="Set1") + scale_colour_brewer(palette="Set1")


# biological examples: genes and pathways
data(genes)
cols <- c(rep("grey80", 24), brewer.pal("YlOrRd", n = 9))
genes$chrom <- factor(genes$chrom, levels=c(paste("chr", 1:22, sep=""), "chrX", "chrY"))
ggparallel(list("path", "chrom"), text.offset=c(0.03, 0,-0.03), data = genes, width=0.1, order=c(1,0), text.angle=0, color="white", factorlevels = c(sapply(unique(genes$chrom), as.character), unique(genes$path))) +
  scale_fill_manual(values = cols, guide="none") + scale_colour_manual(values = cols, guide="none") + coord_flip()


# Titanic
data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$SexSurvived <- with(titanic, interaction(Sex, Survived))
titanic$SexClassSurvived <- with(titanic, interaction(Sex,Class, Survived))
ggparallel(vars=list("Survived", "SexSurvived", "SexClassSurvived"), weight="Freq", data=titanic) + theme(legend.position="none") + scale_fill_manual(values = rep(c("Orange", "Steelblue"), 14)) + scale_colour_manual(values = rep(c("Orange", "Steelblue"), 14))


# ------------------------------------------------------------------------------
# ggparcoord
# ------------------------------------------------------------------------------
libary(GGally)

# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

# use sample of the diamonds data for illustrative purposes
data(diamonds, package="ggplot2")
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 100), ]

# basic parallel coordinate plot, using default settings
p <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10))
p_(p)

# this time, color by diamond cut
p <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2)
p_(p)

# underlay univariate boxplots, add title, use uniminmax scaling
p <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2, scale = "uniminmax", boxplot = TRUE, title = "Parallel Coord. Plot of Diamonds Data")
p_(p)

# utilize ggplot2 aes to switch to thicker lines
p <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2, title ="Parallel Coord. Plot of Diamonds Data", mapping = ggplot2::aes(size = 1)) +
  ggplot2::scale_size_identity()
p_(p)


# basic parallel coord plot of the msleep data, using 'random' imputation and
# coloring by diet (can also use variable names in the columns and groupColumn # arguments)
data(msleep, package="ggplot2")
p <- ggparcoord(data = msleep, columns = 6:11, groupColumn = "vore", missing = "random", scale = "uniminmax")
p_(p)

# center each variable by its median, using the default missing value handler,
# 'exclude'
p <- ggparcoord(data = msleep, columns = 6:11, groupColumn = "vore", scale = "center", scaleSummary = "median")
p_(p)

# with the iris data, order the axes by overall class (Species) separation using the anyClass option
p <- ggparcoord(data = iris, columns = 1:4, groupColumn = 5, order = "anyClass")
p_(p)

# add points to the plot, add a title, and use an alpha scalar to make the lines transparent
p <- ggparcoord(data = iris, columns = 1:4, groupColumn = 5, order = "anyClass", showPoints = TRUE, title = "Parallel Coordinate Plot for the Iris Data", alphaLines = 0.3)
p_(p)

# color according to a column
iris2 <- iris
iris2$alphaLevel <- c("setosa" = 0.2, "versicolor" = 0.3, "virginica" = 0)[iris2$Species]
p <- ggparcoord(data = iris2, columns = 1:4, groupColumn = 5, order = "anyClass", showPoints = TRUE, title = "Parallel Coordinate Plot for the Iris Data", alphaLines = "alphaLevel")
p_(p)

# Use splines on values, rather than lines (all produce the same result)
columns <- c(1, 5:10)
p <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = TRUE)
p_(p)
p <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = 3)
p_(p)
splineFactor <- length(columns) * 3
p <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = I(splineFactor))
p_(p)
