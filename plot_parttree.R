library(rpart)     # For fitting decisions trees
library(parttree)  # This package (will automatically load ggplot2 too)

# install.packages("palmerpenguins")
data("penguins", package = "palmerpenguins")

tree = rpart(species ~ flipper_length_mm + bill_length_mm, data = penguins)
pt = parttree(tree)

## Color palette
# pal = palette.colors(4, "R4")[-1]
pal = hcl.colors(3, "Pastel 1")
pal = hcl.colors(3, "Harmonic")

plot(
  bill_length_mm ~ flipper_length_mm,
  data = penguins, col = pal[species], pch = 19
)
pltrng = par("usr")
rect(
  pmax(pltrng[1], pt$xmin), pmax(pltrng[3], pt$ymin),
  pmin(pltrng[2], pt$xmax), pmin(pltrng[4], pt$ymax),
  col = adjustcolor(pal, alpha.f = 0.1)[pt$species]
)

# plot rectangles only
# dev.new()
# plot(
#   bill_length_mm ~ flipper_length_mm,
#   data = penguins, col = pal[species], pch = 19
# )
# pltrng = par("usr")
# dev.off()
# plot.new()
# plot.window(xlim = pltrng[1:2], ylim = pltrng[3:4])
# rect(
#   pmax(pltrng[1], pt$xmin), pmax(pltrng[3], pt$ymin),
#   pmin(pltrng[2], pt$xmax), pmin(pltrng[4], pt$ymax),
#   col = adjustcolor(pal, alpha.f = 0.1)[pt$species]
# )

# another approach
plot.new()
plot.window(
  xlim = range(penguins[["flipper_length_mm"]], na.rm = TRUE),
  ylim = range(penguins[["bill_length_mm"]], na.rm = TRUE)
)
pltrng = par("usr")
rect(
  pmax(pltrng[1], pt$xmin), pmax(pltrng[3], pt$ymin),
  pmin(pltrng[2], pt$xmax), pmin(pltrng[4], pt$ymax),
  col = adjustcolor(pal, alpha.f = 0.1)[pt$species]
)
axis(1)
axis(2)
box()
title(xlab = "flipper_length_mm")
title(ylab = "bill_length_mm")
# title(main = "my plot")
points(
  bill_length_mm ~ flipper_length_mm,
  data = penguins, col = pal[species], pch = 19
)



pts = lapply(trees, parttree)

## first plot the downscaled image...
plot(pred_img, axes = FALSE)
## ... then layer the partitions as a series of rectangles
pltrng = par("usr")
lapply(
  pts,
  function(pt) rect(
    pmax(pltrng[1], pt$xmin), pmax(pltrng[4], pt$ymin),
    pmin(pltrng[2], pt$xmax), pmin(pltrng[3], pt$ymax),
    lwd = 0.06, border = "grey15"
  )
)


plot(pred_img, axes = FALSE)
plot.new()
plot.window(
  xlim = range(rosalba_ccs[[1]][["x"]]),
  ylim = rev(range(rosalba_ccs[[1]][["y"]]))
  )
pltrng = par("usr")
lapply(
  pts,
  function(pt) rect(
    pmax(pltrng[1], pt$xmin), pmax(pltrng[4], pt$ymin),
    pmin(pltrng[2], pt$xmax), pmin(pltrng[3], pt$ymax),
    lwd = 0.06, border = "grey15"
  )
)

plot.new()
plot(pred_img, axes = FALSE)
lapply(
  1:3,
  function(i) {
    plot.window(
      xlim = range(rosalba_ccs[[i]][["x"]]),
      ylim = rev(range(rosalba_ccs[[i]][["y"]]))
    )
    pltrng = par("usr")
    rect(
      pmax(pltrng[1], pts[[i]]$xmin), pmax(pltrng[4], pts[[i]]$ymin),
      pmin(pltrng[2], pts[[i]]$xmax), pmin(pltrng[3], pts[[i]]$ymax),
      lwd = 0.06, border = "grey15"
    )
  }
)

