####  Part 2: Importing GPS data into R

setwd('C:/Users/pgalante/Documents')

# Create a folder that will act as our directory
dir.create('Projects/QGIS_tutorial/AdvancedGIS_R')

# Read in .csv file of waypoints
wps<-read.csv('Projects/QGIS_tutorial/AdvancedGIS_R/20180413.csv')
# Take a look at the first few rows of the data.frame
head(wps)

# We are only really interested in the first 5 columns of the waypoint file, but let's rearrainge them a bit
wps<-wps[c(3,2,1,4,5)]
# Now lon and lat are the first 2 columns of the data.frame
head(wps)

plot(wps[2:1])
