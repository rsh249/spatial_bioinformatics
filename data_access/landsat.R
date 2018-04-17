install.packages('raster')
library(raster)

## Load in the rasters and turn them into a rasterbrick with 11 bands.
# The bands we downloaded are as follows.
# Band 1  - Coastal aerosol
# Band 2  - Blue
# Band 3  - Green
# Band 4  - Red
# Band 5  - Near Infrared (NIR)
# Band 6  - SWIR1 (short-wave infrared)
# Band 7  - SWIR2
# Band 8  - Panchromatic (detects high-altitude cloud contamination)
# Band 9  - Cirrus (detects high altitude cirrus cloud wavelengths)
# Band 10 - Thermal Infrared (TIRS) 1
# Band 11 - thermal Infrared (TIRS) 2
# Set the working directory to where you saved these rasters
setwd("/pathway/to/your/landsat/rasters")
# Load the rasters as individual bands. You will likely need to change the file names for the rasters you downloaded. B1 = Band 1, etc. Type 
# in the object names and look at the attributes of each one. You might notice that some are different resolutions. Luckily, we aren't interested in
# these rasters, so we will leave these out of further analyses for now.
b1<-raster("LC80160302016111LGN00_B1.TIF")
b2<-raster("LC80160302016111LGN00_B2.TIF")
b3<-raster("LC80160302016111LGN00_B3.TIF")
b4<-raster("LC80160302016111LGN00_B4.TIF")
b5<-raster("LC80160302016111LGN00_B5.TIF")
b6<-raster("LC80160302016111LGN00_B6.TIF")
b7<-raster("LC80160302016111LGN00_B7.TIF")
b8<-raster("LC80160302016111LGN00_B8.TIF")
b9<-raster("LC80160302016111LGN00_B9.TIF")
b10<-raster("LC80160302016111LGN00_B10.TIF")
b11<-raster("LC80160302016111LGN00_B11.TIF")

# Create a rasterbrick from the list of cropped rasters that all have the same extent.
# This may take some time. To save time you can load fewer rasters, or remove some.
# We only need bands 4 and 5. Use rm(c(b1, b2, b3...)) to remove those rasters from R's memory, then modify the code below to accomodate these changes.
LandSat.stack<-stack(c(b1, b2, b3, b4, b5 ,b6 ,b7))

## Learn about the raster package by gaining basic information about image object
print(LandSat.stack)
#This is the same as
LandSat.stack
#Get class information
class(LandSat.stack)
#Get structure information
str(LandSat.stack)
#Get layer names
names(LandSat.stack)
#Get summary statistics
summary(LandSat.stack)
#Get number of layers in the image
nlayers(LandSat.stack)
#Get extent of image
extent(LandSat.stack)
#Plot an image (this may take some time)
plot(LandSat.stack, 4)

## Using this composite multiband rasterbrick, we want to calculate NDVI. 
# Remember: NDVI = (NIR - Red)/ (NIR + Red)
# Bands can be identified using double brackets [[]] (multiband[["LC80160302016111LGN00_B5"]]) to index the brick. Get in the habit of calling
# rasters by their full name (which could be shortened in the folder they in which they are saved). 
# An easy shortcut in RStudio is to hit the tab key within the brackets. This will show you the options of names to choose.
# Remember: in your data will likely have different band names than this example.
NDVI<- (LandSat.stack[["LC80160302016111LGN00_B5"]] - LandSat.stack[["LC80160302016111LGN00_B4"]]) / (LandSat.stack[["LC80160302016111LGN00_B5"]] + LandSat.stack[["LC80160302016111LGN00_B4"]])
# We could have also done this simply by using the R object names that we gave these files earlier
NDVI_alt<- (b5 - b4) / (b5 + b4)

# Plot the NDVI layer
plot(NDVI)
# To save the NDVI raster, use the raster function writeRaster(). Check out writeFormats() to see what type of rasters you can save.
# This function will save the raster into your current working directory.
writeRaster(x = NDVI, filename = "NDVI_R", format='GTiff')


