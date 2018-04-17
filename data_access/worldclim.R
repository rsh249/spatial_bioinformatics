### Downloading bioclimatic variables from Worldclim.org ###
install.packages('dismo')
install.packages('rgdal')
library(dismo)
library(rgdal)

## Set working directory to save bioclim data
setwd('C:/Users/pgalante/Documents/Projects/Global_Wc/Rasters/NorthAmerica')

### when downloading Worldclim data through R, at the 0.5 resolution (30 Arc-seconds- the highest available)
## you can only download data by tile. For our study region, we need to download four tiles, then
## merge them into one large rasterstack, then crop by the extent needed.
# Download each tile for Eastern North America. Be sure to plot one raster from each stack to take a look
# at where each one is. Rasterstacks can be subset using square brackets [ ]. i.e., plot(env[[1]])
env1<-getData(name="worldclim", var="bio", res=0.5, download=T, lat=36, lon=-78)
env2<-getData(name="worldclim", var="bio", res=0.5, download=T, lat=36, lon=-100)
env3<-getData(name="worldclim", var="bio", res=0.5, download=T, lat=20, lon=-78)
env4<-getData(name="worldclim", var="bio", res=0.5, download=T, lat=20, lon=-100)

# Merge the four tiles into one rasterstack called envall (raster functions use the Raster package, which is included in dismo)
env12<-merge(env1, env2)
enva123<-merge(env12, env3)
envall<-merge(env123, env4)

## Define the extent of the study region we want
e<-extent(c(-103.057, -51.625, 15.139, 52.582))
# crop bioclim data to this extent
Bio<-crop(envall, e)

### Now, let's load the occurrence localities from Session 1, and plot them on this map. We will also save these
# rasters as GeoTiffs for future use.
Locs<-readOGR("C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Session1_data", "Locs")
# Add these points on top of a bio layer (using plot(Bio[[1]], or similar command).
points(Locs)

## Let's save these rasters as GeoTiffs. For this we need to use a for loop to iterate through the layers
## of the rasterstack, saving each one with the correct name and extension.
#First, set the working directory to where you want to save the rasters
setwd('C:/Users/pgalante/Documents/Projects/Global_Wc/Rasters/clipped/R')
for (i in 1:nlayers(Bio)){
  # writeRaster is a function that will write each layer with the correct name. As the 
  # loop iterates through, each layer in turn becomes "i". For the names, we will
  # use the names from any of the rasterstacks we downloaded from worldclim
  writeRaster(Bio[[i]], filename = names(env1[[i]]), format='GTiff')
}

## Load rasters back into R. For pattern, the \\ tells R that anything can come before the pattern,
# and the $ tells R that this pattern should only be at the end of the file's name.
bio<-stack(list.files(path = 'C:/Users/pgalante/Documents/Projects/Global_Wc/Rasters/clipped/R', pattern = '\\.tif$', full.names = T))

## In R, manipulating rasters is very easy. Take a look at at the first few values of Bio1:
names(bio[[1]])
head(values(bio[[1]]), 100)
# Remember, temperature values are multiplied by 10 to efficiently store the data (floats are larger). Divide this raster by 10 to see
# the real values for Mean Annual Temperature.
real.temps<-bio[[1]]/10
head(values(real.temps), 100)