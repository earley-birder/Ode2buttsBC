library(tidyverse)
library(sf)

## Read in the kml files
BigWhiteCBC <- sf::st_read("~/Desktop/kmz_tmp/Big_White_CBC_Circle.kml")
BridesvilleCBC <- sf::st_read("~/Desktop/kmz_tmp/Bridesville_CBC_Circle.kml")
CawstonCBC <- sf::st_read("~/Desktop/kmz_tmp/Cawston_CBC_Circle.kml")
KelownaCBC <- sf::st_read("~/Desktop/kmz_tmp/Kelowna_CBC_Circle.kml")
LakeCountryCBC <- sf::st_read("~/Desktop/kmz_tmp/Lake_Country_CBC_Circle.kml")
OliverOsoyoosCBC <- sf::st_read("~/Desktop/kmz_tmp/Oliver_Osoyoos_CBC_Circle.kml")
PeachlandCBC <- sf::st_read("~/Desktop/kmz_tmp/Peachland_CBC_Circle.kml")
PentictonCBC <- sf::st_read("~/Desktop/kmz_tmp/Penticton_CBC_Circle.kml")
VaseuxLakeCBC <- sf::st_read("~/Desktop/kmz_tmp/Vaseux_Lake_CBC_Circle.kml")
VernonCBC <- sf::st_read("~/Desktop/kmz_tmp/Vernon_CBC_Circle.kml")

## make a union for each area
BigWhiteCBC_union <- sf::st_sf(geometry = sf::st_sfc(
  lapply(sf::st_cast(sf::st_union(BigWhiteCBC), "POLYGON"),
         function(x) sf::st_polygon(list(x[[1]]))),
  crs = sf::st_crs(BigWhiteCBC)))
BridesvilleCBC_union <- sf::st_sf(geometry = sf::st_sfc(
  lapply(sf::st_cast(sf::st_union(BridesvilleCBC), "POLYGON"),
         function(x) sf::st_polygon(list(x[[1]]))),
  crs = sf::st_crs(BridesvilleCBC)))
CawstonCBC_union <- sf::st_sf(geometry = sf::st_sfc(
  lapply(sf::st_cast(sf::st_union(CawstonCBC), "POLYGON"),
         function(x) sf::st_polygon(list(x[[1]]))),
  crs = sf::st_crs(CawstonCBC)))
KelownaCBC_union <- sf::st_sf(geometry = sf::st_sfc(
  lapply(sf::st_cast(sf::st_union(KelownaCBC), "POLYGON"),
         function(x) sf::st_polygon(list(x[[1]]))),
  crs = sf::st_crs(KelownaCBC)))
LakeCountryCBC_union <- sf::st_sf(geometry = sf::st_sfc(
  lapply(sf::st_cast(sf::st_union(LakeCountryCBC), "POLYGON"),
         function(x) sf::st_polygon(list(x[[1]]))),
  crs = sf::st_crs(LakeCountryCBC)))
OliverOsoyoosCBC_union <- sf::st_sf(geometry = sf::st_sfc(
  lapply(sf::st_cast(sf::st_union(OliverOsoyoosCBC), "POLYGON"),
         function(x) sf::st_polygon(list(x[[1]]))),
  crs = sf::st_crs(OliverOsoyoosCBC)))
PeachlandCBC_union <- sf::st_sf(geometry = sf::st_sfc(
  lapply(sf::st_cast(sf::st_union(PeachlandCBC), "POLYGON"),
         function(x) sf::st_polygon(list(x[[1]]))),
  crs = sf::st_crs(PeachlandCBC)))
PentictonCBC_union <- sf::st_sf(geometry = sf::st_sfc(
  lapply(sf::st_cast(sf::st_union(PentictonCBC), "POLYGON"),
         function(x) sf::st_polygon(list(x[[1]]))),
  crs = sf::st_crs(PentictonCBC)))
VaseuxLakeCBC_union <- sf::st_sf(geometry = sf::st_sfc(
  lapply(sf::st_cast(sf::st_union(VaseuxLakeCBC), "POLYGON"),
         function(x) sf::st_polygon(list(x[[1]]))),
  crs = sf::st_crs(VaseuxLakeCBC)))
VernonCBC_union <- sf::st_sf(geometry = sf::st_sfc(
  lapply(sf::st_cast(sf::st_union(VernonCBC), "POLYGON"),
         function(x) sf::st_polygon(list(x[[1]]))),
  crs = sf::st_crs(VernonCBC)))

## Project to BC Albers
BigWhiteCBC_proj <- sf::st_transform(BigWhiteCBC_union, 3153)
BridesvilleCBC_proj <- sf::st_transform(BridesvilleCBC_union, 3153)
CawstonCBC_proj <- sf::st_transform(CawstonCBC_union, 3153)
KelownaCBC_proj <- sf::st_transform(KelownaCBC_union, 3153)
LakeCountryCBC_proj <- sf::st_transform(LakeCountryCBC_union, 3153)
OliverOsoyoosCBC_proj <- sf::st_transform(OliverOsoyoosCBC_union, 3153)
PeachlandCBC_proj <- sf::st_transform(PeachlandCBC_union, 3153)
PentictonCBC_proj <- sf::st_transform(PentictonCBC_union, 3153)
VaseuxLakeCBC_proj <- sf::st_transform(VaseuxLakeCBC_union, 3153)
VernonCBC_proj <- sf::st_transform(VernonCBC_union, 3153)

## get the centroid
BigWhiteCBC_centroid <- sf::st_centroid(BigWhiteCBC_union)
BridesvilleCBC_centroid <- sf::st_centroid(BridesvilleCBC_union)
CawstonCBC_centroid <- sf::st_centroid(CawstonCBC_union)
KelownaCBC_centroid <- sf::st_centroid(KelownaCBC_union)
LakeCountryCBC_centroid <- sf::st_centroid(LakeCountryCBC_union)
OliverOsoyoosCBC_centroid <- sf::st_centroid(OliverOsoyoosCBC_union)
PeachlandCBC_centroid <- sf::st_centroid(PeachlandCBC_union)
PentictonCBC_centroid <- sf::st_centroid(PentictonCBC_union)
VaseuxLakeCBC_centroid <- sf::st_centroid(VaseuxLakeCBC_union)
VernonCBC_centroid <- sf::st_centroid(VernonCBC_union)

## get the radius
BigWhiteCBC_radius_km <- units::set_units(max(sf::st_distance(
  sf::st_cast(BigWhiteCBC_union, "POINT"),
  sf::st_centroid(BigWhiteCBC_union))), "km")
BridesvilleCBC_radius_km <- units::set_units(max(sf::st_distance(
  sf::st_cast(BridesvilleCBC_union, "POINT"),
  sf::st_centroid(BridesvilleCBC_union))), "km")
CawstonCBC_radius_km <- units::set_units(max(sf::st_distance(
  sf::st_cast(CawstonCBC_union, "POINT"),
  sf::st_centroid(CawstonCBC_union))), "km")
KelownaCBC_radius_km <- units::set_units(max(sf::st_distance(
  sf::st_cast(KelownaCBC_union, "POINT"),
  sf::st_centroid(KelownaCBC_union))), "km")
LakeCountryCBC_radius_km <- units::set_units(max(sf::st_distance(
  sf::st_cast(LakeCountryCBC_union, "POINT"),
  sf::st_centroid(LakeCountryCBC_union))), "km")
OliverOsoyoosCBC_radius_km <- units::set_units(max(sf::st_distance(
  sf::st_cast(OliverOsoyoosCBC_union, "POINT"),
  sf::st_centroid(OliverOsoyoosCBC_union))), "km")
PeachlandCBC_proj_radius_km <- units::set_units(max(sf::st_distance(
  sf::st_cast(PeachlandCBC_union, "POINT"),
  sf::st_centroid(PeachlandCBC_union))), "km")
PentictonCBC_radius_km <- units::set_units(max(sf::st_distance(
  sf::st_cast(PentictonCBC_union, "POINT"),
  sf::st_centroid(PentictonCBC_union))), "km")
VaseuxLakeCBC_radius_km <- units::set_units(max(sf::st_distance(
  sf::st_cast(VaseuxLakeCBC_union, "POINT"),
  sf::st_centroid(VaseuxLakeCBC_union))), "km")
VernonCBC_radius_km <- units::set_units(max(sf::st_distance(
  sf::st_cast(VernonCBC_union, "POINT"),
  sf::st_centroid(VernonCBC_union))), "km")

## what are those? they should be approx 12.1km
BigWhiteCBC_radius_km
BridesvilleCBC_radius_km
CawstonCBC_radius_km
KelownaCBC_radius_km
LakeCountryCBC_radius_km
OliverOsoyoosCBC_radius_km
PeachlandCBC_proj_radius_km
PentictonCBC_radius_km
VaseuxLakeCBC_radius_km
VernonCBC_radius_km

## Save the simplified unions into new kmls
sf::st_write(BigWhiteCBC_union, 
             "~/Desktop/NathanCBCkmls/BigWhiteCBC_Circle.kml", 
             driver = "KML", delete_dsn = TRUE)
sf::st_write(BridesvilleCBC_union, 
             "~/Desktop/NathanCBCkmls/BridesvilleCBC_Circle.kml", 
             driver = "KML", delete_dsn = TRUE)
sf::st_write(CawstonCBC_union, 
             "~/Desktop/NathanCBCkmls/CawstonCBC_Circle.kml", 
             driver = "KML", delete_dsn = TRUE)
sf::st_write(KelownaCBC_union, 
             "~/Desktop/NathanCBCkmls/KelownaCBC_Circle.kml", 
             driver = "KML", delete_dsn = TRUE)
sf::st_write(LakeCountryCBC_union, 
             "~/Desktop/NathanCBCkmls/LakeCountryCBC_Circle.kml", 
             driver = "KML", delete_dsn = TRUE)
sf::st_write(OliverOsoyoosCBC_union, 
             "~/Desktop/NathanCBCkmls/OliverOsoyoosCBC_Circle.kml", 
             driver = "KML", delete_dsn = TRUE)
sf::st_write(PeachlandCBC_union, 
             "~/Desktop/NathanCBCkmls/PeachlandCBC_Circle.kml", 
             driver = "KML", delete_dsn = TRUE)
sf::st_write(PentictonCBC_union, 
             "~/Desktop/NathanCBCkmls/PentictonCBC_Circle.kml", 
             driver = "KML", delete_dsn = TRUE)
sf::st_write(VaseuxLakeCBC_union, 
             "~/Desktop/NathanCBCkmls/VaseuxLakeCBC_Circle.kml", 
             driver = "KML", delete_dsn = TRUE)
sf::st_write(VernonCBC_union, 
             "~/Desktop/NathanCBCkmls/VernonCBC_Circle.kml", 
             driver = "KML", delete_dsn = TRUE)


