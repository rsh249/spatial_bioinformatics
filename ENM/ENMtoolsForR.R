install.packages("devtools")
library(devtools)
#install_local('C:/Users/pgalante/Documents/R/win-library/3.2/ENMTools-master.zip')
.libPaths('C:/Users/pgalante/Documents/R/win-library/3.2')
library(ENMTools)

env<-stack(list.files(path = 'C:/Users/pgalante/Documents/Projects/Madagascar/layers/TEST', full.names = T, pattern = '\\.tif$'))
env<-setMinMax(env)
rats<- enmtools.species()
names(rats)
rats$species.name<-'majori'
rats$presence.points<-as.data.frame(readRDS("C:/Users/pgalante/Documents/Projects/PJG/locs/locs 1 .RDS"))
rats$range<-background.raster.buffer(rats$presence.points, 50000, mask=env)
rats$background.points<-background.points.buffer(points= rats$presence.points, radius=20000,
                                                 n=1000, mask=env[[1]])
rats
?enmtools.maxent
class(rats$presence.points)



rats.mx<-enmtools.maxent(species=rats, env = env, test.prop = 0.2)
plot(rats.mx$suitability)
raster.breadth(rats.mx)

#################################################################################
setwd()
env.files <- list.files(path = "C:/Users/pgalante/Documents/R/win-library/3.2/ENMTools-master/test/testdata/", pattern = "pc", full.names = TRUE)
env <- stack(env.files)
names(env) <- c("layer.1", "layer.2", "layer.3", "layer.4")
env <- setMinMax(env)
ahli<-enmtools.species()
ahli$species.name <- "ahli"
ahli$presence.points <- read.csv("C:/Users/pgalante/Documents/R/win-library/3.2/ENMTools-master/test/testdata/ahli.csv")[,3:4]
ahli$range <- background.raster.buffer(ahli$presence.points, 50000, mask = env)
ahli$background.points <- background.points.buffer(points = ahli$presence.points,
                                                   radius = 20000, n = 1000, mask = env[[1]])
allogus <- enmtools.species(species.name = "allogus", 
                             presence.points = read.csv("C:/Users/pgalante/Documents/R/win-library/3.2/ENMTools-master/test/testdata/allogus.csv")[,3:4])
allogus$range <- background.raster.buffer(allogus$presence.points, 50000, mask = env)
allogus$background.points <- background.points.buffer(points = allogus$presence.points,
                                                      radius = 20000, n = 1000, mask = env[[1]])
ahli.glm <- enmtools.glm(species = ahli, env = env, f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, test.prop = 0.2)
allogus.glm <- enmtools.glm(species = allogus, env = env, f = pres ~ layer.1 + layer.2 + layer.3 + layer.4, test.prop = 0.2)
visualize.enm(ahli.glm, env, layers = c("layer.2", "layer.4"))
raster.overlap(ahli.glm, allogus.glm)
env.overlap(ahli.glm, allogus.glm, env, tolerance = .001)
id.glm <- identity.test(species.1 = ahli, species.2 = allogus, env = env, type = "glm", nreps = 4)










