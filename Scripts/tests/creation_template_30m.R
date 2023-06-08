library(terra)


template <- rast(extent = ext( -2362395, 15, 221265, 3267405),
                 res = 30, crs = "EPSG:5070", vals = T)
template_DF <- as.data.frame(template, cells = T, xy = T) %>%
  select(cell, x, y)

colnames(template_DF) <- c("FNETID", "cell.aea_x", "cell.aea_y")

save("template_DF", file = "fnet_30m.RData")
