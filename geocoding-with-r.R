### Geocoding and mapping with R ###

# This script corresponds with a blog post
# which can be found at https://www.jessesadler.com/post/geocoding-with-r/

library(tidyverse)
library(ggmap)

# Load the data
letters <- read_csv("data/correspondence-data-1585.csv")

### Geocode the data ###
sources <- distinct(letters, source)
destinations <- distinct(letters, destination)

cities <- full_join(sources, destinations, by = c("source" = "destination"))
cities <- rename(cities, place = source)

##############################################
## Alternative way to get cities data frame ##
##############################################

sources <- letters %>% 
  distinct(source) %>% 
  rename(place = source)

destinations <- letters %>% 
  distinct(destination) %>% 
  rename(place = destination)

cities <- full_join(sources, destinations, by = "place")

# Convert to data frame
cities_df <- as.data.frame(cities)

### Geocode function ###
locations_df <- mutate_geocode(cities_df, place)

# Back to tibble
locations <- as_tibble(locations_df)

library(sf)
library(mapview)

# sf points
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)

# Mapview map
mapview(locations_sf)

# Save locations data
write_csv(locations, "data/locations.csv")

### Mapping with ggmap ###
geocode("mannheim")

map <- get_googlemap(center = c(8.4, 49.5), zoom = 6)

# Black and white styled map
bw_map <- get_googlemap(center = c(8.4, 49.5), zoom = 6,
                        color = "bw",
                        style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")

# Locations map
ggmap(bw_map) +
  geom_point(data = locations, aes(x = lon, y = lat))

### Adding data with dplyr ###
per_source <- letters %>% 
  group_by(source) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

per_destination <- letters %>% 
  group_by(destination) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Join locations to source and destination data
geo_per_source <- left_join(per_source, locations, by = c("source" = "place"))
geo_per_destination <- left_join(per_destination, locations, by = c("destination" = "place"))

## Data map 1 ##
ggmap(bw_map) +
  geom_point(data = geo_per_destination,
             aes(x = lon, y = lat), color = "red") +
  geom_point(data = geo_per_source,
             aes(x = lon, y = lat), color = "purple")

## Data map 2 ##
ggmap(bw_map) +
  geom_point(data = geo_per_destination,
             aes(x = lon, y = lat, size = count),
             color = "red", alpha = 0.5) +
  geom_point(data = geo_per_source,
             aes(x = lon, y = lat, size = count),
             color = "purple", alpha = 0.5)

## Data map 3 ##
ggmap(bw_map) +
  geom_point(data = geo_per_destination,
             aes(x = lon, y = lat, size = count, color = "Destination"), 
             alpha = 0.5) +
  geom_point(data = geo_per_source,
             aes(x = lon, y = lat, size = count, color = "Source"),
             alpha = 0.5) +
  scale_color_manual(values = c(Destination = "red", Source = "purple")) +
  scale_size_continuous(range = c(2, 9)) +
  geom_text_repel(data = locations, aes(x = lon, y = lat, label = place)) +
  labs(title = "Correspondence of Daniel van der Meulen, 1585",
       size = "Letters",
       color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6)))