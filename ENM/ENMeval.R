require(dismo)
require(ENMeval)
require(phyloclim)
require(sp)
require(rgdal)
require(rgeos)
#################################################################################################################
## ENMTools package
# MANUAL: https://cran.r-project.org/web/packages/ENMeval/ENMeval.pdf
# https://github.com/danlwarren/ENMTools
install.packages("devtools")
library(devtools)
#install_local('C:/Users/pgalante/Documents/R/win-library/3.2/ENMTools-master.zip')
.libPaths('C:/Users/pgalante/Documents/R/win-library/3.2')
library(ENMTools)

#################################################################################################################
#### Niche comparisons using Schoener's D. 
# Load two rasters to compare
r1<-raster('Pathwat/to/first/raster.tif') # can be any raster type (i.e. .asci, .tif, .bil)
r2<-raster('Pathway/to/second/raster.tif')
flav<-raster("C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Session6_data/E_m_flavifrons.asc")
mac<-raster("C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Session6_data/E_m_macaco.asc")

# Calculate niche overlap between the rasters. Change stat to 'I' for Moran's I test, and 
# to 'D' for Schoener's D. Remember to use ?nicheOverlap to see the arguments for this function. 
Overlap <- nicheOverlap(r1, r2, stat='I', mask=TRUE, checkNegatives=TRUE)
# This function is used by ENMeval to calculate the niche overlap between candidate models during the
# model tuning process. Set overlap to TRUE to get pairwise Schoener's D values for all candidate models. 

## ENMTools
ENMOverlap<-raster.overlap(flav, mac)
#################################################################################################################
#### Maxent model tuning using ENMeval
# Load environmental rasters as stack. Change pattern to .asc for ascii.
Env<-stack(list.files(path = "Pathway/to/environmental/data", pattern = '\\.tif$', full.names = T))
# Load locality information with 3 columns of "Species", "Longitude", "Latitude"
locs<-read.csv('Pathway/to/locality/data.csv')

# Here, method refers to data partitioning method. Categoricals refers to the names of any environmental data
# that are categorical. fc refers to feature classes to use. bg.coords refers to the background points used every time. You
# can set this as an object (i.e. a .csv file). RMvalues refers to the regularization multipliers to use. 
# Remember to check out ?ENMevaluate
res <- ENMevaluate(occ = locs, env = Env, method='block', categoricals=NULL, 
                   fc = c("L", 'LQ', "H", "LQH"), 
                   bg.coords=NULL, RMvalues=seq(1, 5, 0.5), overlap = T)

###################################################################################################################
#### Identity Test (same as Warren 2008, ENMtools)
setwd('C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Session6_data')
# Load environmental variables
env<-stack(list.files(path='Lemur_layers', pattern = '\\.asc$', full.names=T))
# Load occurrence records for both species
setwd('C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Session6_data') # These should be csv files of records where columns are: "Species, X, Y".
flav <- read.csv("e_m_flav.csv")
maca <- read.csv("e_m_maca.csv")
# Change the species columns to just the species' names
flav[1] <- as.factor('flavifrons')
maca[1] <- as.factor('macaco')
# row bind them so all occurrences are in 3 rows of Species, X, Y
sites<-rbind(maca, flav)
species <- c('flavifrons','macaco')
# Change the column names of sites
colnames(sites)<-c("species","longitude","latitude")
samples <- sites[grep(paste(species, collapse = "|"), sites$species), ]
# Tell R where maxent is (the copy that in with dismo).
maxent.exe <- paste(system.file(package="dismo"),"/java/maxent.jar", sep = "")
### ?niche.equivalency.test
nicheEquivalency<-niche.equivalency.test(p = samples, env = env, app=maxent.exe, dir = 'NicheEquivalence')
# Note that you can also perform the background test using bg.similarity.test. For more see ?bg.similarity.test

####Using ENMTools
# Load environmental variables
env<-stack(list.files(path='C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Session6_data/Lemur_layers', pattern = '\\.asc$', full.names=T))
setwd('C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Session6_data')
## We need all occurrence data and environmental layers to be in decimal degrees, which we can do
# First read in csv data
maca<-read.csv('e_m_maca.csv')[,2:3]
flavi<-read.csv('e_m_flav.csv')[,2:3]
# Now, convert into a SpatialPoints object with the proj4string for the area of interest- here for Madagascar
macasp<-SpatialPoints(maca, proj4string = CRS('+proj=utm +zone=38 +south +ellps=intl +towgs84=-189,-242,-91,0,0,0,0 +units=m +no_defs'))
flavisp<-SpatialPoints(flavi, proj4string = CRS('+proj=utm +zone=38 +south +ellps=intl +towgs84=-189,-242,-91,0,0,0,0 +units=m +no_defs'))
# Convert to lat long for decimal degrees
macadd<-spTransform(macasp, crs('+proj=longlat +datum=WGS84 +no_defs'))
flavidd<-spTransform(flavisp, crs('+proj=longlat +datum=WGS84 +no_defs'))
# Convert back to a dataframe and set the column names
maca.df<-as.data.frame(macadd)
flavi.df<-as.data.frame(flavidd)
colnames(maca.df)<-c('long','lat')
colnames(flavi.df)<-c('long','lat')
# Now we need to change the coordinate reference system of the environmental layers. Set the CRS of this, as before
crs(env)<-'+proj=utm +zone=38 +south +ellps=intl +towgs84=-189,-242,-91,0,0,0,0 +units=m +no_defs'
# Then reproject it to the desired CRS
env2<-projectRaster(env, crs='+proj=longlat +datum=WGS84 +no_defs')
env2<-stack(env2)

env<-stack(list.files(path='Lemur_layers', pattern = '\\.asc$', full.names = T))
flav<-enmtools.species()
flav$species.name <- "flav"
flav$presence.points <- flavi.df
flav$range <- background.raster.buffer(flav$presence.points, 50000, mask = env2)
flav$background.points <- background.points.buffer(points = flav$presence.points,
                                                   radius = 20000, n = 1000, mask = env2[[1]])
mac<-enmtools.species()
mac$species.name <- "mac"
mac$presence.points <- maca.df
mac$range <- background.raster.buffer(mac$presence.points, 50000, mask = env2)
mac$background.points <- background.points.buffer(points = mac$presence.points,
                                                   radius = 20000, n = 1000, mask = env2[[1]])
id.glm <- identity.test(species.1 = flav, species.2 = mac, env = env2, type = "glm", nreps = 4)




#################################################################################################################
#### Get background data from minimum convex polygon of occurrence data.
# Load occurrence data
setwd('pathway/to/occurrence/records')
flav <- read.csv("e_m_flav.csv")
maca <- read.csv("e_m_maca.csv")
# Load environmental data
env <- stack(list.files(path='Pathway/to/session/6/data/Session6_data/Lemur_layers', pattern = '\\.asc$', full.names=T))
# Convert a raster to a polygon shape
envPoly <- rasterToPolygons(env[[1]], fun=NULL, na.rm=T)
# To create a minimum convex polygon, this function need to be sourced. Highlight these lines and run them.
mcp <- function (xy) {
  xy <- as.data.frame(coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.bord))), 1))))
}
# Now run this function on the occurrence records
MCP.locs.flav <- mcp(flav[,2:3])
MCP.locs.maca <- mcp(maca[,2:3])
# Clip the polygon of the entire study region by the MCP polygon
background.area.flav <- gIntersection(envPoly, MCP.locs.flav)
background.area.maca <- gIntersection(envPoly, MCP.locs.maca)
# Convert this back to a raster with the same attributes as the environmental data
MCP.raster.flav <- rasterize(background.area.flav, env[[1]])
MCP.raster.maca <- rasterize(background.area.maca, env[[1]])
# Generate background points from this MCP raster. remember you want 100 times more points than occurrence localities.
bg.points.flav <- randomPoints(mask = MCP.raster.flav, n = (100*nrow(flav)))
bg.points.maca <- randomPoints(mask = MCP.raster.maca, n = (100*nrow(maca)))

###################################################################################################################
#### Background test  Remember to use: ?bg.similarity.test
setwd('pathway/to/occurrence/records')
flav <- read.csv("e_m_flav.csv")
maca <- read.csv("e_m_maca.csv")
# Change the species columns to just the species' names
flav[1] <- as.factor('flavifrons') # Change this to your species 1's name
maca[1] <- as.factor('macaco') # Change this to your species 2's name
# row bind them so all occurrences are in 3 rows of Species, X, Y
sites<-rbind(maca, flav)
species <- c('flavifrons','macaco')
# Change the column names of sites
colnames(sites)<-c("species","longitude","latitude")
samples <- sites[grep(paste(species, collapse = "|"), sites$species), ]
# Load environmental data
env <- stack(list.files(path='Pathway/to/session/6/data/Session6_data/Lemur_layers', pattern = '\\.asc$', full.names=T))
env.flav<-mask(env, MCP.raster.flav)
env.maca<-mask(env, MCP.locs.maca)
# Tell R where maxent is (the copy that in with dismo).
maxent.exe <- paste(system.file(package="dismo"),"/java/maxent.jar", sep = "")
# set the working directory
setwd('Pathway/to/save/test/Session6_data')
# Perform background similarity test. Remember to use ?bg.similarity.test
bg.test <- bg.similarity.test(p = samples, env = env, app = maxent.exe, dir = 'background', n=2)

#### ENMTools
# Load environmental variables
env<-stack(list.files(path='C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Session6_data/Lemur_layers', pattern = '\\.asc$', full.names=T))
setwd('C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Session6_data')
## We need all occurrence data and environmental layers to be in decimal degrees, which we can do
# First read in csv data
maca<-read.csv('e_m_maca.csv')[,2:3]
flavi<-read.csv('e_m_flav.csv')[,2:3]
# Now, convert into a SpatialPoints object with the proj4string for the area of interest- here for Madagascar
macasp<-SpatialPoints(maca, proj4string = CRS('+proj=utm +zone=38 +south +ellps=intl +towgs84=-189,-242,-91,0,0,0,0 +units=m +no_defs'))
flavisp<-SpatialPoints(flavi, proj4string = CRS('+proj=utm +zone=38 +south +ellps=intl +towgs84=-189,-242,-91,0,0,0,0 +units=m +no_defs'))
# Convert to lat long for decimal degrees
macadd<-spTransform(macasp, crs('+proj=longlat +datum=WGS84 +no_defs'))
flavidd<-spTransform(flavisp, crs('+proj=longlat +datum=WGS84 +no_defs'))
# Convert back to a dataframe and set the column names
maca.df<-as.data.frame(macadd)
flavi.df<-as.data.frame(flavidd)
colnames(maca.df)<-c('long','lat')
colnames(flavi.df)<-c('long','lat')
# Now we need to change the coordinate reference system of the environmental layers. Set the CRS of this, as before
crs(env)<-'+proj=utm +zone=38 +south +ellps=intl +towgs84=-189,-242,-91,0,0,0,0 +units=m +no_defs'
# Then reproject it to the desired CRS
env2<-projectRaster(env, crs='+proj=longlat +datum=WGS84 +no_defs')
env2<-stack(env2)
env<-stack(list.files(path='Lemur_layers', pattern = '\\.asc$', full.names = T))
flav<-enmtools.species()
flav$species.name <- "flav"
flav$presence.points <- flavi.df
flav$range <- background.raster.buffer(flav$presence.points, 50000, mask = env2)
flav$background.points <- background.points.buffer(points = flav$presence.points,
                                                   radius = 20000, n = 1000, mask = env2[[1]])
mac<-enmtools.species()
mac$species.name <- "mac"
mac$presence.points <- maca.df
mac$range <- background.raster.buffer(mac$presence.points, 50000, mask = env2)
mac$background.points <- background.points.buffer(points = mac$presence.points,
                                                  radius = 20000, n = 1000, mask = env2[[1]])

bg.mx.asym <- background.test(species.1 = mac, species.2 = flav, env = env2, type = "mx", nreps = 4, test.type = "asymmetric" )

