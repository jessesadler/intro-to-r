### An Exploration of Simple Features for R: Building sfg, sfc, and sf objects from the sf package ###

# This script goes along with the blog post of the same name,
# which can be found at https://www.jessesadler.com/post/simple-feature-objects/
# See the Rmarkdown document for the contents of the blog post.

# Load sf package
library(sf)

#################
## sfg objects ##
#################

# Create an sfg object with coordinates of Los Angeles and Amsterdam
la_sfg <- st_point(c(-118.2615805, 34.1168926))
amsterdam_sfg <- st_point(c(4.8979755, 52.3745403))

# Structure of a sfg POINT object
str(la_sfg)

# Coordinates of a sfg object
st_coordinates(la_sfg)

# Create MULTIPOINT and LINESTRING sfg objects through matrix of points
multipoint_sfg <- st_multipoint(rbind(c(-118.2615805, 34.1168926), c(4.8979755, 52.3745403)))
linestring_sfg <- st_linestring(rbind(c(-118.2615805, 34.1168926), c(4.8979755, 52.3745403)))

# Plot to compare MULTIPOINT to LINESTRING
par(mar = c(0, 1, 2, 1),
    mfrow = c(1, 2))
plot(multipoint_sfg, main = "MULTIPOINT")
plot(linestring_sfg, main = "LINESTRING")

# Distance on sfg objects does not have real-world meaning
st_distance(la_sfg, amsterdam_sfg)

# st_distance and stats::dist the same for sfg objects
dist(rbind(c(-118.2615805, 34.1168926), c(4.8979755, 52.3745403)))

#################
## sfc objects ##
#################

# Create sfc object with default crs
st_sfc(multipoint_sfg)

# Create sfc object with multiple sfg objects
points_sfc <- st_sfc(la_sfg, amsterdam_sfg, crs = 4326)

# Attributes of sfc object
attributes(points_sfc)

# crs attribute is of class crs
class(st_crs(points_sfc))

# Access crs attribute of sfc object
st_crs(points_sfc)

# Geographic distance with sfc object
st_distance(points_sfc)

################
## sf objects ##
################

# Create a data frame of attributes
data <- data.frame(name = c("Los Angeles", "Amsterdam"),
                   language = c("English", "Dutch"),
                   weather = c("sunny", "rainy/cold"))

# Create data frame with list column then make sf object
st_sf(cbind(data, points_sfc))

# Make sf object from separate data frame and sfc objects
st_sf(data, geometry = points_sfc)

# Create sf object by combining data frame with sfc object and name sfc column geometry
points_sf <- st_sf(data, geometry = points_sfc)

# Class of new sf object
class(points_sf)

# Geometry of points_sf is equivalent to points_sfc
identical(st_geometry(points_sf), points_sfc)

# Stickiness of geometry column
dplyr::select(points_sf, name)

# Return sf object to a data frame by setting geometry to NULL
st_set_geometry(points_sf, NULL)

# Class
class(st_set_geometry(points_sf, NULL))