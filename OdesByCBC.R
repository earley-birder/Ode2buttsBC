## Load your packages into the library so that 
## your computer knows which packages to prioritize
library(tidyverse)
library(red)
library(rinat)
library(sf)


## Read in Gibson data
GibsonDF <- read.csv("S0008347X24000385sup002.csv")

## Remove rows with NA Lat Long data
GibsonDF <- GibsonDF %>% 
  dplyr::filter(!is.na(Longitude), !is.na(Latitude))

GibsonBC <- GibsonDF %>% 
  dplyr::filter(Prov_State == "British Columbia")

GibsonBC_sf <- sf::st_as_sf(
  GibsonBC,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)



VaseuxCentroid <- sf::st_sfc(
  sf::st_point(c(-119.592627, 49.308124)),  # lon, lat
  crs = 4326
)

VaseuxCentroid_proj <- sf::st_transform(VaseuxCentroid, 3005)  # EPSG:3005 = BC Albers

VaseuxCBCcircle <- sf::st_buffer(VaseuxCentroid_proj, dist = 12000)

VaseuxCBCcircle_wgs84 <- sf::st_transform(VaseuxCBCcircle, 4326)

plot(VaseuxCBCcircle_wgs84, border = "red", lwd = 2)
plot(VaseuxCentroid, add = TRUE, pch = 16)

ApexCentroid <- sf::st_sfc(
  sf::st_point(c(-119.911581, 49.308790)),  # lon, lat
  crs = 4326
)

ApexCentroid_proj <- sf::st_transform(ApexCentroid, 3005)  # EPSG:3005 = BC Albers

ApexCBCcircle <- sf::st_buffer(ApexCentroid_proj, dist = 12000)

ApexCBCcircle_wgs84 <- sf::st_transform(ApexCBCcircle, 4326)

plot(ApexCBCcircle_wgs84, border = "red", lwd = 2)
plot(ApexCentroid, add = TRUE, pch = 16)

OliverCentroid <- sf::st_sfc(
  sf::st_point(c(-119.521697, 49.085022)),  # lon, lat
  crs = 4326
)

OliverCentroid_proj <- sf::st_transform(OliverCentroid, 3005)  # EPSG:3005 = BC Albers

OliverCBCcircle <- sf::st_buffer(OliverCentroid_proj, dist = 12000)

OliverCBCcircle_wgs84 <- sf::st_transform(OliverCBCcircle, 4326)

plot(OliverCBCcircle_wgs84, border = "red", lwd = 2)
plot(OliverCentroid, add = TRUE, pch = 16)



plot(VaseuxCBCcircle_wgs84, border = "red", lwd = 2)
plot(ApexCBCcircle_wgs84, add = TRUE, border = "red", lwd = 2)
plot(OliverCBCcircle_wgs84, add = TRUE, border = "red", lwd = 2)


## Now make new dfs for records that land in the circle
VaseuxRecords <- GibsonBC_sf %>% 
  dplyr::filter(sf::st_intersects(., VaseuxCBCcircle_wgs84, sparse = FALSE)[, 1])
ApexRecords <- GibsonBC_sf %>% 
  dplyr::filter(sf::st_intersects(., ApexCBCcircle_wgs84, sparse = FALSE)[, 1])
OliverRecords <- GibsonBC_sf %>% 
  dplyr::filter(sf::st_intersects(., OliverCBCcircle_wgs84, sparse = FALSE)[, 1])

## Some summary stats
nrow(VaseuxRecords)
nrow(ApexRecords)
nrow(OliverRecords)

length(unique(VaseuxRecords$ItemName))
length(unique(ApexRecords$ItemName))
length(unique(OliverRecords$ItemName))

VaseuxSppCounts <- VaseuxRecords %>%
  group_by(ItemName) %>%
  summarise(count = n()) 

ggplot(VaseuxSppCounts, aes(x = count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    x = "Number of records per Species",
    y = "Number of Species",
    title = "Distribution of Records per Species (Vaseux)"
  ) +
  theme_minimal()

ApexSppCounts <- ApexRecords %>%
  group_by(ItemName) %>%
  summarise(count = n()) 

ggplot(ApexSppCounts, aes(x = count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    x = "Number of records per Species",
    y = "Number of Species",
    title = "Distribution of Records per Species (Apex)"
  ) +
  theme_minimal()

OliverSppCounts <- OliverRecords %>%
  group_by(ItemName) %>%
  summarise(count = n()) 

ggplot(OliverSppCounts, aes(x = count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    x = "Number of records per Species",
    y = "Number of Species",
    title = "Distribution of Records per Species (Oliver)"
  ) +
  theme_minimal()
