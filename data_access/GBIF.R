### Downloading occurrence data from GBIF and plotting on a map ###
install.packages('dismo')
install.packages('rgdal')
install.packages('GISTools')
library(dismo)
library(rgdal)
library(maps)

## Go to www.naturalearthdata.com > Get the Data > Medium Scale Data; Cultural > Download countries.
# then load the shapefile back into R.
continents<-readOGR(dsn = "C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Session1_data", layer = "ne_50m_admin_0_countries")
# Subset the SpatialPolygonsDataset by North America. Try plotting this and see how it looks. 
N.America<-continents[continents@data$continent=="North America",]
# crop North America by a rough extent of the area of interest.
Amb.opa<-crop(N.America, extent(c(-99, -66, 46, 23)))
# Plot this extent of the map. Use any color you like.
plot(Amb.opa, col='blue')

# Query GBIF for data associated with species name
MarbSalam<- gbif(genus = "Ambystoma", species = "opacum", download = T)
# Look at the first 5 rows of MarbSalam. Get only the columns that have the species name, latitude and longitude.
Locs<-na.omit(data.frame(cbind(MarbSalam$species), MarbSalam$lon, MarbSalam$lat))
# Rename the column names of the Locs data.frame
colnames(Locs)<-c("SPECIES","LONGITUDE","LATITUDE")
# Put these points on the map
points(Locs[,2:3], col='red', pch=16)
# Add a legend
legend("bottomright", col='yellow', pch=16, legend = 'Ambystoma opacum')
# Add a scale
map.scale(x = -98, y = 23, relwidth=0.25, metric=T)
# load GISTools library. This will mask the map.scale function from the "maps" package. Do this step last.
# Add north arrow.
library(GISTools)
north.arrow(xb=-67, yb = 30, len=0.5, lab="N")


