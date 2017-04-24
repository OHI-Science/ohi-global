#################################
## Multipart figures
#################################

library(ggplot2)
library(grid)
library(gridExtra)
library(png)    # reading in pre-existing figures

p1 <- qplot(mpg, wt, data=mtcars, colour=cyl)
p2 <- qplot(mpg, data=mtcars)

grid.arrange(p1, p2)

grid.arrange(p1, p2, ncol=1)


################################
## Incorporating grobs
################################

t1 <- textGrob("hello")

d <- head(iris, 3)
g <- tableGrob(d)

Index_map <- rasterGrob(readPNG("global2016/Reporting/figures/maps_by_goal_mol/global_map_Index_2016_mol.png"), interpolate=TRUE)

grid.arrange(p1, p2, t1, g, Index_map, ncol=3)

####################################
## Layouts using grid.arrange
####################################

## set up a layout grid
lay <- rbind(c(1, 1, 1, 1, 2, 2, 3),
             c(1, 1, 1, 1, 2, 2, 3),
             c(4, 4, 4, 4, 5, 5, 5),
             c(4, 4, 4, 4, 5, 5, 5))
grid.arrange(p1, p2, t1, g, Index_map, layout_matrix=lay)


## dimensions
grid.arrange(p1, p2, t1, g, Index_map, widths=c(8,3,1), heights=c(5,5))

## save the image
png("test.png", width=1000, height=500)
grid.arrange(p1, p2, t1, g, Index_map, widths=c(8,3,1), heights=c(5,5))
dev.off()
