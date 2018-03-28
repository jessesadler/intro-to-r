## Great circles with sp and sf ##

# This script goes along with the blog post of the same name,
# which can be found at https://www.jessesadler.com/post/great-circles-sp-sf/
# See the Rmarkdown document for the contents of the blog post.

# Load packages
library(tidyverse)
library(sp)
library(geosphere)
library(sf)
library(rnaturalearth)

# Load the data
letters <- read_csv("data/correspondence-data-1585.csv")
locations <- read_csv("data/locations.csv")

# Load the background maps
countries_sp <- ne_countries(scale = "medium")
countries_sf <- ne_countries(scale = "medium", returnclass = "sf")

####################################
## Great circle vs Rhumb line map ##
####################################

# Create points data
la_sfg <- st_point(c(-118.2615805, 34.1168926))
amsterdam_sfg <- st_point(c(4.8979755, 52.3745403))
points_sfc <- st_sfc(la_sfg, amsterdam_sfg, crs = 4326)
points_data <- data.frame(name = c("Los Angeles", "Amsterdam"))
points_sf <- st_sf(points_data, geometry = points_sfc)

# Create lines
rhumb_line <- st_linestring(rbind(c(-118.2615805, 34.1168926), c(4.8979755, 52.3745403))) %>% 
  st_sfc(crs = 4326)

# Make a great circle line from LA to Amsterdam as sfc object
great_circle <- st_linestring(rbind(c(-118.2615805, 34.1168926), c(4.8979755, 52.3745403))) %>% 
  st_sfc(crs = 4326) %>% 
  st_segmentize(units::set_units(50, km))

# Combine two sfc objects
lines_sfc <- c(rhumb_line, great_circle)

# Labels and transformation to sf object
lines_data <- data.frame(name = c("Rhumb line", "Great circle"))
lines_sf <- st_sf(lines_data, geometry = lines_sfc)

# Background map
map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(sovereignt)

ggplot() +
  geom_sf(data = map, fill = gray(0.7), color = gray(0.7)) +
  geom_sf(data = lines_sf, aes(color = name), size = 1.5, show.legend = FALSE) +
  geom_sf(data = points_sf, aes(color = name), size = 3, show.legend = FALSE) +
  coord_sf(xlim = c(-120, 20), ylim = c(20, 70)) +
  theme_minimal()

########################
## Preparing the data ##
########################

# Create data frame of routes and count for letters per route
routes <- letters %>%
  group_by(source, destination) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(n)

# Print routes
routes

##########################################
## SpatialLinesDataFrame with geosphere ##
##########################################

# tibble of longitude and latitude values of sources
sources_tbl <- routes %>% 
  left_join(locations, by = c("source" = "place")) %>% 
  select(lon, lat)

# tibble of longitude and latitude values of destinations
destinations_tbl <- routes %>% 
  left_join(locations, by = c("destination" = "place")) %>% 
  select(lon, lat)

# Great circles as a SpatialLines object
routes_sl <- gcIntermediate(sources_tbl, destinations_tbl, 
                            n = 50, addStartEnd = TRUE, 
                            sp = TRUE)

# Class of routes_sl
class(routes_sl)

# Slots in routes_sl
slotNames(routes_sl)

# CRS of routes_sl
routes_sl@proj4string

# Make bbox of countries_sp the same as routes_sl
countries_sp@bbox <- bbox(routes_sl)

# Plot map
par(mar = c(1, 1, 3, 1))
plot(countries_sp, col = gray(0.8), border = gray(0.7),
     main = "SpatialLines great circles")
plot(routes_sl, col = "dodgerblue", add = TRUE)

# Great circles as a SpatialLinesDataFrame object
routes_sldf <- SpatialLinesDataFrame(routes_sl, data = routes)

# Map with SpatialLinesDataFrame object
par(mar = c(1, 1, 3, 1))
plot(countries_sp, col = gray(0.8), border = gray(0.7),
     main = "SpatialLinesDataFrame great circles")
plot(routes_sldf,
     lwd = sqrt(routes_sldf$n/3) + 0.25,
     col = "dodgerblue",
     add = TRUE)

########################################
## Great circles with sf: tidy method ##
########################################

# Add id column to routes
routes_id <- rowid_to_column(routes, var = "id")

# Transform routes to long format
routes_long <- routes_id %>% 
  gather(key = "type", value = "place", source, destination)

# Add coordinate values
routes_long_geo <- left_join(routes_long, locations, by = "place")

# Convert coordinate data to sf object
routes_long_sf <- st_as_sf(routes_long_geo, coords = c("lon", "lat"), crs = 4326)

# Convert POINT geometry to MULTIPOINT, then LINESTRING
routes_lines <- routes_long_sf %>% 
  group_by(id) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

# Print routes_lines
routes_lines

# Join sf object with attributes data
routes_lines <- left_join(routes_lines, routes_id, by = "id")

# Convert rhumb lines to great circles
routes_sf_tidy <- routes_lines %>% 
  st_segmentize(units::set_units(20, km))

# Compare number of points in routes_lines and routes_sf_tidy
nrow(st_coordinates(routes_lines))
nrow(st_coordinates(routes_sf_tidy))

# Rhumb lines vs great circles
ggplot() +
  geom_sf(data = countries_sf, fill = gray(0.8), color = gray(0.7)) +
  geom_sf(data = routes_lines, color = "magenta", show.legend = "point") + 
  geom_sf(data = routes_sf_tidy) + 
  coord_sf(xlim = c(2, 14), ylim = c(45, 54), datum = NA) + 
  ggtitle("Rhumb lines vs Great circles") + 
  theme_minimal()

# Interactive comparison of gcIntermediate and st_segmentize
library(mapview)

mapview(routes_sldf, color = "magenta") + 
  mapview(routes_sf_tidy, color = "black")

############################################
## Great circles with sf: for loop method ##
############################################

# Create a line between Venice and Haarlem
st_linestring(rbind(c(12.315515, 45.44085), c(4.646219, 52.38739)))

# Matrix of longitude and latitude values of sources
sources_m <- routes %>% 
  left_join(locations, by = c("source" = "place")) %>% 
  select(lon:lat) %>% 
  as.matrix()

# Matrix of longitude and latitude values of destinations
destinations_m <- routes %>% 
  left_join(locations, by = c("destination" = "place")) %>% 
  select(lon:lat) %>% 
  as.matrix()

# Create empty list object of length equal to number of routes
linestrings_sfg <- vector("list", nrow(routes))

# Define sequence and body of loop
for (i in 1:nrow(routes)) {
  linestrings_sfg[[i]] <- st_linestring(rbind(sources_m[i, ], destinations_m[i, ]))
}

# sfc object of great circles
linestrings_sfc <- st_sfc(linestrings_sfg, crs = 4326) %>% 
  st_segmentize(units::set_units(20, km))

# Print linestrings_sfc
linestrings_sfc

# Create sf object from data frame and sfc geometry set
routes_sf <- st_sf(routes, geometry = linestrings_sfc)

# Show routes_sf_tidy and routes_sf are equivalent
select(routes_sf_tidy, -id) %>% 
  all.equal(routes_sf)

# ggplot2 of great cirlce routes
ggplot() +
  geom_sf(data = countries_sf, fill = gray(0.8), color = gray(0.7)) +
  geom_sf(data = routes_sf, aes( color = n)) + 
  scale_color_viridis_c() +
  labs(color = "Letters", title = "Great circles with sf") + 
  coord_sf(xlim = c(2, 14), ylim = c(45, 54), datum = NA) + 
  theme_minimal()