# ---------------
# read shapefile
# ---------------
library(rgdal)
myShapeInR <- readOGR(".", "OD_Zones_Internal")
plot(myShapeInR)

#
# http://www.r-bloggers.com/shapefiles-in-r/
#
library(maptools)
area <- readShapePoly("serpm653se.shp")
plot(area)
#print(area)

library(RColorBrewer)
colors <- brewer.pal(9, "BuGn")

# MS coordinates 32.3547° N, 89.3985° W
# CHECK WHY IT'S PRINTING CALIFORNIA MAP ?????
library(ggmap)
mapImage <- get_map(location = c(lon = -27, lat = 82),
                    color = "color",
                    source = "osm")

#Next, we can use the fortify function from the ggplot2 package. This converts the crazy shape file with all its nested attributes into a data frame that ggmap will know what to do with.

area.points <- fortify(area)
plot(area.points)

#Finally, we can map our shape files!
ggmap(mapImage) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = area.points,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")

