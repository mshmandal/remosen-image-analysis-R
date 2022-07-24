################################################################################
# Script       : Import Landsat 8 data and do a change detection analysis
# Author       : Mohammad Shamim Hasan Mandal
# Date         : July 24 2022
# Comment      : ----
################################################################################

# Project  : Remote sensing change detection analysis between two years 2014-2020
# Date     : Landsat 8 Surface Reflectance images

# Workflow : 
# 1. We collect Landsat8 SR data from NASA https://earthexplorer.usgs.gov/
# 2. We read the file susing Raster pakcage
# 3. We resacle the values
# 4. Crop and mask to our study area extent
# 5. We calculate NDVI (Normalized Difference Vegeation Index)
# NDVI values is a popular remote sensing index, values range between -1 to 1
# where high values more than 0.3 typically means vegetation. 
# Values from 0 to less than zero means water
# values less than 0.3 and more than 0 can be urban area, soil surface etc.

# Our research: we want to see changes in vegetation in Wari-bateshwar area
# we will consider entire Narsingdi district for our study area. Because often
# these changes become visible at landscape scale

# Example studies:
# Chyla, J.M. How Can Remote Sensing Help in Detecting the Threats to 
# Archaeological Sites in Upper Egypt? Geosciences 2017, 7, 97. 
# https://doi.org/10.3390/geosciences7040097 

browseURL("https://www.mdpi.com/2076-3263/7/4/97")
################################################################################
# first up clean R environment to start fresh
gc()
rm(list = ls())
 

# load library
library(raster)
library(sf)


################################################################################
# Import data
################################################################################

# get a shapefile
bd = getData(name = "GADM",country ="Bangladesh",level = 1,download = TRUE,
                  path = "./data")
bd = st_as_sf(bd) # make sf object
# only dhaka division
dhaka = bd[bd$NAME_1 == "Dhaka",]
Narsingdi_elv = raster("./data/Narsingdi_elv.tif")
Narsingdi_shp = st_read("./wari/Narsingdi.shp")
st_crs(Narsingdi_shp) = 4326

################################################################################
# Import Landsat8 images
################################################################################
# get the file location
browseURL("https://earthexplorer.usgs.gov/")

# use file.choose()
l8_2014_dir = "./landsat/LC08_L2SP_137043_20140314_20200911_02_T1/"
l8_2020_dir = "./landsat/LC08_L2SP_137043_20220304_20220314_02_T1/"

# list files in a directory
(l8_2014_files = list.files(path=l8_2014_dir,pattern = ".TIF",full.names = T))
(l8_2020_files = list.files(path=l8_2020_dir,pattern = ".TIF",full.names = T))

# read the files, only select from Band 1 to Band 7
(l8_2014_stack = stack(l8_2014_files[3:9]))
(l8_2020_stack = stack(l8_2020_files[3:9]))

# rename 
(names(l8_2014_stack) = gsub(pattern = "LC08_L2SP_137043_20140314_20200911_02_T1_SR_","",names(l8_2014_stack)))
(names(l8_2020_stack) = gsub(pattern = "LC08_L2SP_137043_20220304_20220314_02_T1_SR_","",names(l8_2020_stack)))

# check the rasters
l8_2014_stack
l8_2020_stack

################################################################################
# CRS mismatch
################################################################################
# plot first band
plot(Narsingdi_shp$geometry)
plot(l8_2014_stack$B1,add=T)

# check the crs
crs(Narsingdi_shp)
crs(l8_2014_stack)

# reproject
nar_utm = st_transform(Narsingdi_shp, crs = crs(l8_2014_stack))

# plot first band
plot(l8_2014_stack$B1,main="Band1 of Landsat")
plot(nar_utm$geometry,add=T)

################################################################################
# Cropping and masking
################################################################################
l8_2014_stack <- crop(l8_2014_stack,nar_utm)
l8_2020_stack <- crop(l8_2020_stack,nar_utm)

# check the first layer
plot(l8_2014_stack$B1,main="B1 of Landsat")
plot(nar_utm$geometry,add=T)
################################################################################
# Rescaling
################################################################################
# read about the data
#browseURL("https://www.usgs.gov/centers/eros/science/usgs-eros-archive-landsat-archives-landsat-8-9-olitirs-collection-2-level-2")

# here we only use the surface reflectance; more information here
#browseURL("https://www.usgs.gov/landsat-missions/landsat-collection-2-surface-reflectance")

# product user guide
#browseURL("https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/media/files/LSDS-1619_Landsat-8-9-C2-L2-ScienceProductGuide-v4.pdf")

# Scale the values; this may take time
# browseURL("https://www.usgs.gov/faqs/how-do-i-use-scale-factor-landsat-level-2-science-products")

(l8_2014_stack = (l8_2014_stack*0.0000275) + -0.2)
(l8_2020_stack = (l8_2020_stack*0.0000275) + -0.2)

################################################################################
# Band combinations
################################################################################
#Band 2	Visible blue	0.450 to 0.515 µm	30 meter
#Band 3	Visible green	0.525 to 0.600 µm	30 meter
#Band 4	Visible red	0.630 to 0.680 µm	30 meter
#Band 5	Near-infrared	0.845 to 0.885 µm	30 meter
#Band 6	Short wavelength infrared	1.56 to 1.66 µm	30 meter
#Band 7	Short wavelength infrared	2.10 to 2.30 µm	60 meter

# clear map
dev.off()

# True color or Natural Color (4, 3, 2)
plotRGB(x=l8_2014_stack,
        # RGB meaning Red Green Blue
        r=4,
        g=3,
        b=2,
        stretch='hist'
        )

# False color or Color Infrared (5, 4, 3)
plotRGB(x=l8_2014_stack,
        # RGB meaning Red Green Blue
        r=5,
        g=3,
        b=2,
        stretch='hist',
)

# Agriculture (6, 5, 2)
plotRGB(x=l8_2014_stack,
        # RGB meaning Red Green Blue
        r=6,
        g=5,
        b=2,
        stretch='hist',
)
################################################################################
# Calculate NDVI
################################################################################

# NDVI = (NIR-RED)/(NIR+RED)

ndvi2014 = (l8_2014_stack$B5- l8_2014_stack$B4)/(l8_2014_stack$B5 + l8_2014_stack$B4)
ndvi2020 = (l8_2020_stack$B5- l8_2020_stack$B4)/(l8_2020_stack$B5 + l8_2020_stack$B4)

# real values must be between 0-1
ndvi2014
ndvi2020

# change between the two time period
diff = ndvi2020 - ndvi2014

# Plot the two raster
par(mfrow=c(1,2),mar=c(4,4,4,4))
plot(ndvi2014)
plot(ndvi2014)
dev.off()

# the difference image
plot(diff,main="The difference between 2014-2020")
plot(nar_utm$geometry,add=T)

# thresholding values
hist(diff,main="Histogran of chagne NDVI values")


# if we assume that a change of +- 0.2 is not a significant change; so that
# we only want know the areas with big fluctations of NDVI changes
thres = diff
thres[thres < 0.2 & thres  >= 0 ] = NA # values between 0 to 0.2 gets NA
thres[thres > -0.2 & thres <= 0 ] = NA  # values between 0 to -0.2 gets NA


library(RColorBrewer)
par(mfrow=c(1,2),mar=c(4,4,4,4))
pal= brewer.pal(10,"BrBG")
plot(thres,main="Default color plot")
plot(thres,
     breaks=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1),
     col=pal,
     main="Custom color map"
     )
dev.off()

################################################################################
# Home work
################################################################################
# Read about raster image procssing 
browseURL("rspatial.org/")

# homework-1
# try to save the raster
# save the maps as pdf,png

# homework-2
# Try to plot wari shapefile with the NDVI change; 
# hint: probably the crs is not correct???
dev.off()
wari = st_read("./wari/wari garh_poly.shp")
st_crs(wari) = 32645   # this is not the write coordinate system
plot(thres)
plot(wari,add=T)

