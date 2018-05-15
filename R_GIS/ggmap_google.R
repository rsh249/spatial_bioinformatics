library(ggmap)
loc = cbind(c(-74.0440, -74.0441, -74.038), c(40.618, 40.620, 40.635))
loc = as.data.frame(loc)
colnames(loc) = c('lon', 'lat')

bkbox <- make_bbox(lon = loc$lon, lat = loc$lat, f = .1)
bkmap <- get_map(location = bkbox, maptype = "satellite", source = "google", zoom =14)
ggmap(bkmap) + 
  geom_point(data = loc, 
             color = "red",
             size =4)


ll_means <- sapply(loc[1:2], mean)
bkmap2 <- get_map(location = ll_means,  maptype = "satellite", source = "google", zoom = 15)

ggmap(bkmap2) + 
  geom_point(data = loc, 
             color = "red",
             size =4)

bkmap3 <- get_map(location = ll_means,  maptype = "terrain", source = "google", zoom = 10)


ggmap(bkmap3) + 
  geom_point(data = loc, 
             color = "red",
             size =4)


##With mapdata data
library(tidyverse)
library(mapdata)
library(maps)

whr <- map_data("worldHires") %>%
  as_tibble()

ggplot() +
  geom_polygon(data = whr, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  coord_quickmap(xlim = c(-75, -74), ylim = c(40, 41)) + 
  geom_point(data = loc, aes(x=lon, y =lat),
             color = "red",
             size =4) +
  theme_bw()

