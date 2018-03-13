### Introduction to GIS with R: Spatial data with the sp and sf packages ###

# This script goes along with the blog post of the same name,
# which can be found at https://www.jessesadler.com/post/gis-with-r-intro/
# See the Rmarkdown document for the contents of the blog post.

### Load packages and data
library(tidyverse)
library(sp)
library(sf)
library(rnaturalearth)

# Load the data
letters <- read_csv("data/correspondence-data-1585.csv")
locations <- read_csv("data/locations.csv")

########################
## Preparing the data ##
########################

# Letters per source
sources <- letters %>% 
  group_by(source) %>% 
  count() %>% 
  rename(place = source) %>% 
  add_column(type = "source") %>% 
  ungroup()

# Letters per destination
destinations <- letters %>% 
  group_by(destination) %>% 
  count() %>% 
  rename(place = destination) %>% 
  add_column(type = "destination") %>% 
  ungroup()

# Bind the rows of the two data frames
# and change type column to factor
letters_data <- rbind(sources, destinations) %>% 
  mutate(type = as_factor(type))

# Join letters_data to locations
geo_data <- left_join(letters_data, locations, by = "place")

##################################
## Spatial data with sp package ##
##################################

# Create data frame of only longitude and latitude values
coords <- select(geo_data, lon, lat)

# Create SpatialPoints object with coords and CRS
points_sp <- SpatialPoints(coords = coords,
                           proj4string = CRS("+proj=longlat +datum=WGS84"))

# Create SpatialPointsDataFrame object
points_spdf <- SpatialPointsDataFrame(coords = coords,
                                      data = letters_data,  
                                      proj4string = CRS("+proj=longlat +datum=WGS84"))

# Example of subsetting `points_spdf` to return locations with "n" greater than 10
points_spdf[points_spdf@data$n > 10, ]

# Get coastal and country world maps as Spatial objects
coast_sp <- ne_coastline(scale = "medium")
countries_sp <- ne_countries(scale = "medium")

####################################
## Mapping with sp and base plots ##
####################################

### Set up plot ###

# Create a new color palette to distinguish source and destination
palette(alpha(c("darkorchid", "darkorange"), 0.7))
# Set margins for bottom, left, top, and right of plot
par(mar = c(1, 1, 3, 1))

### Plot points ###
plot(points_spdf,
     pch = 20,
     col = points_spdf$type,
     cex = sqrt(points_spdf$n)/2 + 0.25)

# Add a box around the plot
box()
# Add a title
title(main = "Correspondence of Daniel van der Meulen, 1585")

### Plot map with coastlines (lines) data ###
# Pointsize vector for legend
pointsize <- c(1, 50, 100)
par(mar = c(1, 1, 3, 1))

# Plot points
plot(points_spdf,
     pch = 20,
     col = points_spdf$type,
     cex = sqrt(points_spdf$n)/2 + 0.25)
# Plot coastlines background map
plot(coast_sp,
     col = "black",
     add = TRUE)
# Add a box around the plot
box()

# Legend for colors
legend("topright", legend = levels(points_spdf$type),
       pt.cex = 2,
       col = 1:2,
       pch = 15)

# legend for size of points
legend("right", legend = pointsize,
       pt.cex = (sqrt(pointsize)/2 + 0.25),
       col = "black",
       pch = 20,
       title = "Letters")

# Title for the map
title(main = "Correspondence of Daniel van der Meulen, 1585")

# Make bounding box for countries_sp match
# bounding box of points_spdf
countries_sp@bbox <- bbox(points_spdf)

### Plot map with countries (polygons) data ###
par(mar = c(1, 1, 3, 1))

# Plot countries map and color with grays
plot(countries_sp,
     col = gray(0.8),
     border = gray(0.7))
# Plot points
plot(points_spdf,
     pch = 20,
     col = points_spdf$type, 
     cex = sqrt(points_spdf$n)/2 + 0.25,
     add = TRUE)
# Add a box around the plot
box()

# Legend for colors
legend("topright",
       legend = levels(points_spdf$type),
       pt.cex = 2,
       col = 1:2,
       pch = 15)
# legend for size of points
legend("right",
       legend = pointsize,
       pt.cex = (sqrt(pointsize)/2 + 0.25),
       col = "black",
       pch = 20,
       title = "Letters")

# Title for the map
title(main = "Correspondence of Daniel van der Meulen, 1585")

##################################
## Spatial data with sf package ##
##################################

# Create sf object with geo_data data frame and CRS
points_sf <- st_as_sf(geo_data, coords = c("lon", "lat"), crs = 4326)

# Get coastal and country world maps as sf objects
coast_sf <- ne_coastline(scale = "medium", returnclass = "sf")
countries_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Subset of locations with "n" greater than 10
filter(points_sf, n > 10)

### Subset of countries object to get South American countries ###
# South American countries with new CRS
countries_sf %>% 
  filter(continent == "South America") %>% 
  select(name) %>% 
  st_transform(crs = "+proj=moll +datum=WGS84")

### Make map of South America ###
# Return to default palette
palette("default")

# Map of South American countries
countries_sf %>% 
  filter(continent == "South America") %>% 
  select(name) %>% 
  st_transform(crs = "+proj=moll +datum=WGS84") %>% 
  plot(key.pos = NULL, graticule = TRUE, main = "South America")

#################################
## Mapping with sf and ggplot2 ##
#################################

### Basic ggplot2 plot with geom_sf ###
ggplot() + 
  geom_sf(data = coast_sf) + 
  geom_sf(data = points_sf,
          aes(color = type, size = n),
          alpha = 0.7,
          show.legend = "point") +
  coord_sf(xlim = c(-1, 14), ylim = c(44, 55))

### Plot map with coastlines (lines) data ###
# Load ggrepel package
library(ggrepel)

ggplot() + 
  geom_sf(data = coast_sf) + 
  geom_sf(data = points_sf, 
          aes(color = type, size = n), 
          alpha = 0.7, 
          show.legend = "point") +
  coord_sf(xlim = c(-1, 14), ylim = c(44, 55),
           datum = NA) + # removes graticules
  geom_text_repel(data = locations, 
                  aes(x = lon, y = lat, label = place)) +
  labs(title = "Correspondence of Daniel van der Meulen, 1585",
       size = "Letters",
       color = "Type",
       x = NULL,
       y = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6))) +
  theme_minimal()

### Plot map with countries (polygons) data ###
ggplot() + 
  geom_sf(data = countries_sf,
          fill = gray(0.8), color = gray(0.7)) + 
  geom_sf(data = points_sf, 
          aes(color = type, size = n), 
          alpha = 0.7, 
          show.legend = "point") +
  coord_sf(xlim = c(-1, 14), ylim = c(44, 55),
           datum = NA) + # removes graticules
  geom_text_repel(data = locations, 
                  aes(x = lon, y = lat, label = place)) +
  labs(title = "Correspondence of Daniel van der Meulen, 1585",
       size = "Letters",
       color = "Type",
       x = NULL,
       y = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6))) +
  theme_bw()