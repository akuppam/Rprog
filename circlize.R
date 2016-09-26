library(circlize)
library(reshape)

setwd("~/Pythprog/Rprog")

tourdest <- read.csv("tourdest.csv", stringsAsFactors = F)
summary(tourdest)
head(tourdest)

#Convert the table to matrix and delete the first column
tourdest2 <- data.matrix(subset(tourdest, select = -c(1)))
summary(tourdest2)
head(tourdest2)

# set Matrix Row and Column names
rownames(tourdest2) <- tourdest$x
colnames(tourdest2) <- colnames(tourdest2)

# View the Matrix
View(tourdest2)

# ***********************
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# To use for fills, add
scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
scale_colour_manual(values=cbPalette)
# ***********************

# Initialize grid colors
grid.col = NULL

# Set row and column labels colors
grid.col[colnames(tourdest)] = "grey"
grid.col[rownames(tourdest)] = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Parameters for circos layout. The gap.degree specifies the gap between two
# neighbour sectors. It can be a single value or a vector. If it is a
# vector, the first value corresponds to the gap after the first sector

circos.par(gap.degree = 8)

tourdest2 <- t(tourdest2)
# The chordDiagram command draws the links between O/D pairs. For details on
# what each parameter means see the tutorial document linked above.
chordDiagram(tourdest, grid.col = grid.col, directional = TRUE, annotationTrack = "grid", 
             preAllocateTracks = list(list(track.height = 0.05), list(track.height = 0.05)))

#circos.initialize(factors = factors, xlim = xlim)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.index = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), mean(ylim), sector.index, facing = "inside", niceFacing = TRUE)
}, bg.border = NA)

circos.clear()

# START WITH A 2X2 MATRIX ######
