#!/usr/bin/env Rscript
###########################################################
#
# Relationship between crime and CCTV presence in Baltimore
#
# Keith Hughitt <khughitt@umd.edu>
# 2013/02/05
#
# The goal of this script is to create a simple plot relating
# number of arrests and prescence of CCTV cameria in Baltimore
# city. This is part of a class exercise for the CMSC 702 at
# UMD taught by Héctor Corrada Bravo.
#
# Note that the GDAL library is needed in order to install the rgdal
# binding.
#
# Datasets used:
# --------------
# 1. https://data.baltimorecity.gov/Crime/BPD-Arrests/3i3v-ibrt
# 2. https://data.baltimorecity.gov/Crime/CCTV-Locations/hdyb-27ak
# 3. https://data.baltimorecity.gov/Geographic/Neighborhoods/5cni-ybar
#
# References:
# -----------
# https://data.baltimorecity.gov/browse?limitTo=datasets
# http://www.r-bloggers.com/visualizing-baltimore-with-r-and-ggplot2-crime-data/
#
###########################################################
library(maptools)
library(sp)
library(rgdal)
library(spatstat)
library(ggmap)
library(rgeos)

# Load data
arrests = read.csv('data/BPD_Arrests.csv')
cameras = read.csv('data/CCTV_Locations.csv')

# Clean up arrests dataset
names(arrests)[3] = "race"
names(arrests)[4] = "sex"
arrests = arrests[arrests$Location.1 != "",]

# Parse coordinates
arrest_coords = parse_coords(arrests)
camera_coords = parse_coords(cameras)

# Draw a map of Baltimore
baltimore = plot_bmore()

# Plot 2d histogram of arrests
# could also use geom_bin2d, geom_density2d, etc.            
baltimore + geom_point(data=camera_coords, aes(x=long, y=lat, color='black')) +
            geom_hex(data=arrest_coords, aes(x=long, y=lat, alpha=0.5)) +
            scale_fill_gradient(low="blue", high="red")

###########################################################
#
# parse_coords
#
# A simple function to parse coordinates from data.baltimore.gov datasets
# I'm sure there is a better way to do this...
#
###########################################################
parse_coords = function(df, col_name='Location.1') {
    # split coords
    tmp = gsub('[() ]', '', unlist(strsplit(as.character(df[,col_name]), ',')))
    
    # convert to data frame and return
    df = data.frame(t(matrix(as.numeric(tmp), 2)))
    names(df) = c('lat', 'long')
    
    # reorder and add a group column for better ggplot2 compat
    df = cbind(df[c('long', 'lat')], group=0)

    return(df)
}

###########################################################
#
# plot_bmore
#
# Draw a map of Baltimore city, including outlines for the different 
# neighborhoods.
#
# Source:
# http://www.r-bloggers.com/visualizing-baltimore-with-r-and-ggplot2-crime-data/
#
###########################################################
plot_bmore = function () {
    city_shp = readOGR(dsn='maps/baltcity_line.dbf', layer='baltcity_line')
    city_shp <- spTransform(city_shp, CRS("+proj=longlat +datum=WGS84"))
    city_pl_df <- fortify(city_shp, region='LABEL')

    bound_plot <- ggplot(data=city_pl_df, aes(x=long, y=lat, group=group)) +
                  geom_polygon(color='gray', fill='lightblue') + 
                  coord_equal() + theme_nothing()

    nbhds_df <- read.dbf('maps/nhood_2010.dbf')
    nbhds_shp <- readOGR(dsn='maps/nhood_2010.dbf', layer='nhood_2010')
    nbhds_shp <- spTransform(nbhds_shp, CRS("+proj=longlat +datum=WGS84"))
    nbhds_pl_df <- fortify(nbhds_shp, region='LABEL')
    nbhds_plot <- bound_plot + geom_polygon(data=nbhds_pl_df, color='gray',
                                            fill='white')

    return(nbhds_plot)
}


