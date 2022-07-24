################################################################################
# Script       : Import data files in R
# Author       : Mohammad Shamim Hasan Mandal
# Date         : July 24 2022
# Comment      : ----
################################################################################
# first up clean R environment to start fresh
gc()
rm(list = ls())

# Here in this script we will use raster package and sf package to make
# a map for whole of Bangladesh. Then we will use SRTM elevation data
# to 

# load library
library(raster) # for image handling
library(sf)     # for shapefile

################################################################################
# SET PROJECT DIRECTORY
################################################################################
# First we need to select in which folder we have our data or which
# folder we will use for saving our data/results.

getwd() # this shows current working directory

# say we want to make a project directory under "C:/" drive (Windows OS)
# and name the project as "RemoSen-Image-Analysis-in-R-Basics" then we 
# can do that by following commands

# remove "#" hash from the following code and run, then rerun getwd() to check
# setwd("C:/RemoSen-Image-Analysis-in-R-Basics")

# now check again
getwd()  


# now we create two addition folder under our project directory to save our data
# and our results
dir.create("./result")
dir.create("./data")

################################################################################
# RASTER package getData() function
################################################################################

# Raster package has getData() function from which we can download data from 
# GADM and Worldclim websites
# see the function documentation
?getData()


# Use the getData() function and save the results of this function as an variable
# named "bd". Then inside the getData() function we give additional arguments
# think this as additional instructions as per the function documentation
bd = getData(
  # the first argument is the name of the data source
  name = "GADM", 
  # because we selected "GADM" so, as from the getData() function documentation
  # we can additionally set country argument
  country ="Bangladesh",
  # we can also use level argument to determines which admin level data we want
  level = 1,             # this is an integer as function documentation 
  download = TRUE,       # after download set this to FALSE
  path = "./data"
  )

# Now check the data to see if this has been downloaded.
bd                       # you can do the same using print(bd)
crs(bd)                  # coordinate reference system (crs) in WGS84
# check more information; use for getting the EPSG code
browseURL("https://epsg.io/4326") 

# Now, the first information we see that this is a "SpatialPolygonsDataFrame"
# class type data. For our purpose we want it to be a sf (simple feature) class.
# we can do this by using sf package st_as_sf() function
library(sf)
bd = st_as_sf(bd)        # and save as the same name

# now check the class
class(bd)


# Now plot the data
plot(
  x= bd,                # what to plot, x is what?
  axes=T,               # shows the grid references
)

################################################################################
# SAVING maps or plots
################################################################################

# We can save this map in PNG/JPG or PDF format
pdf(file = "./result/bd_map.pdf",width = 6,height = 3)
plot(bd,axes=T)         # this is draw the plot
dev.off()               # this line finishes the process and is a must

# similarly we can make png and jpg file. Here image sizes are in pixels
# so we need to increase the width and height argument
png(file = "./result/bd_map.png",width = 1000,height = 500)
plot(bd,axes=T)
dev.off()

# if you prefer jpg image files 
jpeg(file = "./result/bd_map.jpg",width = 1000,height = 500)
plot(bd,axes=T)
dev.off()

################################################################################
# Sub setting sf class objects and modifying our maps
################################################################################

# Our bd variables is same like data.frame object
head(bd)
tail(bd)

# check the structures
str(bd)

# the bd object has 11 variables and 7 observations; think variables like data 
# columns and each values are organized by rows.

# we can get variable names
names(bd)

# We see we have a column "NAME_0" which is the admin level 0 : meaning whole of
# Bangladesh. Next we have "NAME_1" meaning Admin Level 1 divisions. Lets check 
plot(bd["NAME_0"]$geometry)
plot(bd["NAME_1"]$geometry)

# We can count the values in a particular cell using table() function
table(bd$NAME_0)
table(bd$NAME_1)

# We can we plot Dhaka divion map
dhaka = bd[bd$NAME_1 == "Dhaka",]

# if more than one region
dhaka_chattogram = bd[bd$NAME_1 %in% c("Dhaka","Chittagong"),]

plot(dhaka$geometry,axes=T)
plot(dhaka_chattogram$geometry,axes=T)

################################################################################
# Get elevation raster
################################################################################
?getData
elv = getData(
  name = "alt",
  country="Bangladesh",
  mask=TRUE,
  download = TRUE,
  path="./data"
  )

# print the object
elv

# Plot with default col and breaks
plot(elv, main="Bangladesh mean elevation")


################################################################################
# Cropping and masking raster
################################################################################
# check the crs of the elv raster
crs(elv)

# as both elv and dhaka shapefile has WGS84 we can Dhaka shapefile/vector to
# crop/mask our elv to the extent of dhaka

# cropping will use the extent of dhaka: which is a bounding box around dhaka
# vector; so, the elv raster basically crop as a rectangle
dhaka_elv = crop(elv, dhaka)
plot(dhaka_elv,main="Mean elelvation of Dhaka")
plot(dhaka$geometry,add=T)

# if we want to remove the values outside dhaka region we can mask out all the
# values outside Dhaka vector
dhaka_elv = mask(elv, dhaka)
plot(dhaka_elv,main="Mean elelvation of Dhaka")
plot(dhaka$geometry,add=T)

################################################################################
# Wari-Bateshwar ruins
################################################################################
# read Wari-Bateshwar shapefile
wari = st_read("./wari/wari garh_poly.shp")
plot(wari,main="Wari Bateshwar area vector/shapefile")

# check crs
crs(wari)  # no crs

# assign wgs84
wari_wgs84 = wari
st_crs(wari_wgs84) = 4326   # this is not the write coordinate system
plot(wari_wgs84)

# assign utm 45n
# browseURL("https://epsg.io/32645")
wari_utm45 = wari
crs(wari_utm45)
st_crs(wari_utm45) = 32645
plot(wari_utm45)

# now if we wanted to assign WGS84 coordinate system: meaning transforming the
# utm45 crs system object to WGS84, we need to tansform the vector/shapefile
wari_trans_to_wgs84 = st_transform(wari_utm45,crs = 4326)
crs(wari_trans_to_wgs84)
plot(wari_trans_to_wgs84)

################################################################################
# Elevation map of Wari-Bateshwar ruins
################################################################################
# get district level admin boundary data
bd_2 = getData(
  name = "GADM", country ="Bangladesh",
  level = 2,             # this is an integer as function documentation 
  download = TRUE,       # after download set this to FALSE
  path = "./data"
)

# convert to sf object
bd_2 = st_as_sf(bd_2)
table(bd_2$NAME_2)

# get only Narshingdi distrcit
Narsingdi = bd_2[bd_2$NAME_2== "Narsingdi",] 
plot(Narsingdi$geometry)


# crop the raster
(Narsingdi_elv = crop(elv,Narsingdi))
(Narsingdi_elv_mask = mask(Narsingdi_elv,Narsingdi))

# plot the rasters
par(mfrow=c(1,2),mar=c(2,2,2,2))
plot(Narsingdi_elv,main="Narsingdi elv cropped")
plot(Narsingdi_elv_mask,main="Narsingdi elv masked")

dev.off()

################################################################################
# Saving vector and raster data
################################################################################

# vector data
st_write( Narsingdi,"./wari/Narsingdi.shp",append = F)

# Raster data
writeRaster(
  x = Narsingdi_elv_mask,
  filename = "./data/Narsingdi_elv.tif",
  overwrite=T
  )



