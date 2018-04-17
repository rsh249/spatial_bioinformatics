install.packages('dismo')
install.packages('rgdal')
install.packages('rJava')
library(rJava)
library(dismo)
library(rgdal)

## We can fairly easily create a Maxent model in R. However, to do this you must:
# 1. Download Maxent from http://www.cs.princeton.edu/~schapire/maxent/
# 2. Copy the file Maxent.jar and paste a copy of it in ".../R(version)/library/dismo/java". 
#    There should already be "dismo.jar" in that folder. Add Maxent..ar to it

# Create a rasterStack of your bioclim data
Env <- stack(list.files('Pathway/to/your/environmental/data', full.names = T, pattern = ".tif$"))
# Load locality information as two columns of longitude, latitude.
locs<- read.csv("Pathway/to/your/locality/information/species.csv")

###Create that optimal model using all localities (no withheld data)
###x = rasterstack, p = locs object as data.frame, a = background coords, factors = categorical variables, 
### make true only the arguments wanted. E.g. using autofeatures lets the algorithm choose features based on the
# number of occurrences. Otherwise, features are used by default. You can use a feature by changing it to 'true'.
mod <- maxent(
  x=Env, # bio stack
  p=locs, # locality csv
  factors = NULL,
#  path= "Pathway/to/save/your/model/files", # path to save to
  args=c(
    'betamultiplier=1',
    #'linear=false',
    #'quadratic=false',
    #'product=false',
    #'threshold=false',
    #'hinge=false',
    'threads=2',
    'responsecurves=true',
    'jackknife=true',
    'askoverwrite=false',
    'autofeature=true'
  )
)

# Create the raster prediction. This may be a different geographic area or time than the model was created using.
Pred.Mod<- predict(
  object = mod,
  x = Env,
  #filename = "Pathway/to/save/prediction/as/raster", #where do you want the prediction saved?
  na.rm = T,
  format = 'ascii',#or GTiff
  overwrite = F,
  args = "logistic"
)







