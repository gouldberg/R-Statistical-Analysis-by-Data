# ------------------------------------------------------------------------------
# 可視化:  対散布図
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# GGally::ggpairs
# ------------------------------------------------------------------------------
library(GGally)

# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

# Quick example, with and without colour
data(flea)
ggpairs(flea, columns = 2:4)

pm <- ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species))
p_(pm)

# Note: colour should be categorical, else you will need to reset
# the upper triangle to use points instead of trying to compute corr

data(tips, package = "reshape")
pm <- ggpairs(tips[, 1:3])
p_(pm)

pm <- ggpairs(tips, 1:3, columnLabels = c("Total Bill", "Tip", "Sex"))
p_(pm)

pm <- ggpairs(tips, upper = "blank")
p_(pm)



## Plot Types
# Change default plot behavior
pm <- ggpairs(
  tips[, c(1, 3, 4, 2)],
  upper = list(continuous = "density", combo = "box_no_facet"),
  lower = list(continuous = "points", combo = "dot_no_facet"))
p_(pm)


pm <- ggpairs(
  tips[, c(1, 3, 4, 2)],
  upper = list(continuous = ggally_density, combo = ggally_box_no_facet),
  lower = list(continuous = ggally_points, combo = ggally_dot_no_facet) )
p_(pm)


data(diamonds, package="ggplot2")
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 1000), ]
# Different aesthetics for different plot sections and plot types
pm <- ggpairs( diamonds.samp[, 1:5], mapping = ggplot2::aes(color = cut),
  upper = list(continuous = wrap("density", alpha = 0.5), combo = "box_no_facet"),
  lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4)),
  title = "Diamonds" )
p_(pm)

## Axis Label Variations
# Only Variable Labels on the diagonal (no axis labels)
pm <- ggpairs(tips[, 1:3], axisLabels="internal")
p_(pm)
# Only Variable Labels on the outside (no axis labels)
pm <- ggpairs(tips[, 1:3], axisLabels="none")
p_(pm)

## Facet Label Variations
# Default
df_x <- rnorm(100)
df_y <- df_x + rnorm(100, 0, 0.1)
df <- data.frame(x = df_x, y = df_y, c = sqrt(df_x^2 + df_y^2))
pm <- ggpairs( df, columnLabels = c("alpha[foo]", "alpha[bar]", "sqrt(alpha[foo]^2 + alpha[bar]^2)") )
p_(pm)

# Parsed labels:
pm <- ggpairs( df, columnLabels = c("alpha[foo]", "alpha[bar]", "sqrt(alpha[foo]^2 + alpha[bar]^2)"), labeller = "label_parsed" )
p_(pm)


## Plot Insertion Example
custom_car <- ggpairs(mtcars[, c("mpg", "wt", "cyl")], upper = "blank", title = "Custom Example")
plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg, label=rownames(mtcars)))
plot <- plot + ggplot2::geom_text(ggplot2::aes(colour=factor(cyl)), size = 3) + ggplot2::scale_colour_discrete(l=40)
custom_car[1, 2] <- plot
personal_plot <- ggally_text( "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---" )
custom_car[1, 3] <- personal_plot
p_(custom_car)


data(flea)
ggscatmat(flea, columns = 2:4)
