## PLZ-Shapefile mit Gemeinde-shapefile intersecten,
## Gemeinde-Einwohnerzahlen flächengewichtet den Intersects zuweisen,
## und resultierenden Datensatz ohne Geometrie exportieren.
## Damit können GISD-Scores dann einwohnerproportional den PLZ-Gebieten zugeordnet werden.

## Gebietsstand ist analog zum Hauptskript der 31.12.2022
## (PLZ sind von Stand 2024, aber die Änderungen sind vernachlässigbar)

# Autor*in: Lola Omar Soliman (auf Basis von Lars Kroll)
# Datum: 2025-01-16

# Pakete laden
library(tidyverse)  # Tidyverse Methoden
library(sf)         # Geospatial Data Manipulation
library(rmapshaper) # Simplify shapefiles

# Skriptpfad als Arbeitspfad setzen
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Dezimal-Zahlendarstellung erzwingen
options(scipen=999)

# Jahr des Gebietsstands vermerken
gebietsstand <- 2022

# Gemeinde-Shapefile inkl. Einwohnerzahlen laden (Gebietsstand: Ende 2022, Projektion: GK3)
# (Quelle: https://daten.gdz.bkg.bund.de/produkte/vg/vg250-ew_ebenen_1231/2022/vg250-ew_12-31.gk3.shape.ebenen.zip)
shp_gem <- st_read("vg250-ew_12-31.gk3.shape.ebenen/vg250-ew_ebenen_1231/VG250_GEM.shp") %>%
  mutate(bundesland = floor(as.numeric(as.character(AGS))/1000000)) %>% 
  select(gemeinde_id = AGS,
         gemeinde_name = GEN,
         population_gem = EWZ,
         geometry)

# PLZ-Shapefile laden (Gebietsstand: April 2024, Projektion: UTM32)
# (Quelle: GeoBasis-DE (leider nur auf Anfrage verfügbar))
shp_plz <- st_read("PLZ (intern)/plz/de/PLZ.shp") %>% 
  # Projektion an Gemeinde-Shapefile angleichen
  st_transform(st_crs(shp_gem)) %>%
  # PLZ auf optionale 4, 3, 2, 1 Ziffern auflösen
  mutate(PLZ5 = as.numeric(PLZ_5),
         PLZ4 = floor(PLZ5/10),
         PLZ3 = floor(PLZ4/10),
         PLZ2 = floor(PLZ3/10),
         PLZ1 = floor(PLZ2/10)) %>% 
  select(-PLZ_5)

# Shape simplifizieren
shp_plz_small <- ms_simplify(shp_plz, keep=.1 , keep_shapes=T)
shp_plz <- shp_plz_small
rm(shp_plz_small)

# PLZ mit Gemeinden intersecten und Einwohner der Gemeinden flächengewichtet auf Intersections aufteilen
shp_intersect_gem_plz <- st_intersection(shp_gem, shp_plz) %>% 
  mutate(flaeche_intersect = as.numeric(st_area(.))) %>% 
  group_by(gemeinde_id) %>% 
  mutate(flaeche_proportion = flaeche_intersect/sum(flaeche_intersect),
         population_intersect = round(flaeche_proportion*population_gem)) %>%
  ungroup() %>% 
  select(gemeinde_id,
         gemeinde_name,
         population_gem,
         flaeche_intersect,
         flaeche_proportion,
         population_intersect,
         contains("PLZ")) %>% 
  arrange(gemeinde_id, PLZ5) %>% 
  # Geometrie entfernen
  st_set_geometry(NULL) %>% 
  # Exportieren
  saveRDS(., paste0("EW_Gem_PLZ_Intersect_",gebietsstand,".rds"))

