####  Part 2: Importing GPS data into R
require(raster);require(sp);require(rgdal); require(ggmap)

###  Set your working directory
setwd('C:/Users/pgalante/Documents')

# Create a folder that will act as our directory and move into it
dir.create('AdvancedGIS_R')
setwd('AdvancedGIS_R')

###############################################################
############  Importing cellphone data  #######################
###############################################################
# Read in .csv file of waypoints
wps<-read.csv('/home/pjg/GIS/Classes/spatial_bioinformatics/R_GIS/20180413.csv')
# Take a look at the first few rows of the data.frame
head(wps)
# We are only really interested in the first 5 columns of the waypoint file, but let's rearrainge them a bit
wps<-wps[c(3,2,1,4,5)]
# Now lon and lat are the first 2 columns of the data.frame
head(wps)

##############################################################
###############  Importing Garmin GPS data  ##################
##############################################################
###### Create a character vector of the entire pathway for the gpx object from your Garmin GPS
gpfile<-"/home/pjg/GIS/Classes/spatial_bioinformatics/R_GIS/Current.gpx"
###### Read in the GPX file using the function from rdgal
GPSfile<-readOGR(dsn = gpfile, layer = "track_points")
##### Extract all of the data and coordinates
wpsDF<-GPSfile@data
wpsCoords<-GPSfile@coords
##### Pull out only the necessary data:longitude, latitude, time, elevation
##### Garmin does not supply accuracy at each waypoint
wps<-cbind(wpsCoords[,1:2], wpsDF[,c(5,4)])
colnames(wps)<-c("lon", "lat", "time", "elev")        

###############################################################
############  Visualizing and creating features  ##############
###############################################################
##  Visualizing your waypoints with satellite data
# Find the bounding box of your waypoints
loc = cbind(c(min(wps$lon), max(wps$lon)), c(min(wps$lat), max(wps$lat)))
# Name the columns
loc = as.data.frame(loc)
colnames(loc) = c('lon', 'lat')
# Get the images from Google Earth Engine
manbox <- make_bbox(lon = loc$lon, lat = loc$lat, f = .1)
manmap <- get_map(location = manbox, maptype = "satellite", source = "google", zoom =17)
# Plot
ggmap(manmap) + 
  geom_point(data = wps, 
             color = "red",
             size =1) # for better resolution, change size to smaller

##  Transform the waypoints and save as a shapefile
wpsShp<-SpatialPointsDataFrame(coords = wps[,1:2], data = wps)
writeOGR(obj = wpsShp, dsn = paste(getwd()), layer = "Allwaypoints", driver = "ESRI Shapefile")

##  Separate the data into the appropriate features from which waypoints were taken outside
# The first step is to take another look at the data and find the rownumber of the corresponding feature
print(wps)
# Enter the row number of the point feature below
PointFeat<- 1
# Enter the row numbers of the line features below, separated with a colon
LineFeat<- 2:10
# As above, enter the row numbers of the polygon features below, separated with a colon  
PolyFeat<-  11:22 
# Now, use these row numbers to create individual 'spatial' objects for each of the features
PointShape <- SpatialPointsDataFrame(wps[,1:2][PointFeat,], data = wps[PointFeat,])
PointLine <- SpatialLinesDataFrame(sl = SpatialLines(list(Lines(list(Line(coords = wps[,1:2][LineFeat,])), ID='a'))), match.ID = F, data = as.data.frame("Line"))
PointPoly <- SpatialPolygonsDataFrame(Sr = SpatialPolygons(list(Polygons(list(Polygon(wps[PolyFeat,][,1:2])),"Poly"))), data = data.frame("Poly"), match.ID = F)

# And save as ESRI shapefiles
writeOGR(obj = PointShape, dsn = paste(getwd()), layer = "PointFeature", driver = "ESRI Shapefile")
writeOGR(obj = PointLine, dsn = paste(getwd()), layer = "LineFeature", driver = "ESRI Shapefile")
writeOGR(obj = PointPoly, dsn = paste(getwd()), layer = "PolyFeature", driver = "ESRI Shapefile")

## Test for accuracy
# PointShape
ggmap(manmap) + 
  geom_point(data = data.frame(PointShape@coords), 
             color = "red",
             size =1) # for better resolution, change size to smaller

# LineShape
ggmap(manmap) + 
  geom_polygon(data = PointLine, 
               aes(x = long, y = lat),
               color = "red",
               size =1) # for better resolution, change size to smaller

# PolyShape
ggmap(manmap) + 
  geom_polygon(data = PointPoly, 
               aes(x = long, y = lat),
               color = "red",
               size =1) # for better resolution, change size to smaller
