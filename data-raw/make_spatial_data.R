

# load libraries ----------------------------------------------------------

library(sf)
library(tidyverse)
library(arcpullr)
library(devtools)


# deer mgt units ----------------------------------------------------------

# DMUs
dmus <-
  get_spatial_layer('https://dnrmaps.wi.gov/arcgis/rest/services/WM_CWD/WM_DMU_MSU_DMZ_Ext/MapServer/2') %>%
  select(-OBJECTID, -SHAPE.AREA, -SHAPE.LEN) %>%
  janitor::clean_names()


# counties ----------------------------------------------------------------

counties <-
  get_spatial_layer('https://dnrmaps.wi.gov/arcgis/rest/services/DW_Map_Dynamic/EN_Basic_Basemap_WTM_Ext_Dynamic_L16/MapServer/3') %>%
  select(-OBJECTID, -SHAPE.AREA, -SHAPE.LEN, -DNR_CNTY_CODE, -COUNTY_FIPS_CODE) %>%
  janitor::clean_names()


# ecological landscapes ---------------------------------------------------

ecological_landscapes <-
  get_spatial_layer('https://dnrmaps.wi.gov/arcgis/rest/services/DW_Map_Dynamic/EN_Forest_Land_Cover_WTM_Ext/MapServer/3') %>%
  select(-OBJECTID, -SHAPE.AREA, -SHAPE.LEN, -ECO_LANDSCAPE_ID) %>%
  janitor::clean_names()


# turkey management zones -------------------------------------------------

turkey_mgt_zones <-
  get_spatial_layer('https://dnrmaps.wi.gov/arcgis/rest/services/DW_Map_Dynamic/EN_Hunting_Zones_WTM_Ext/MapServer/3') %>%
  select(-OBJECTID, -SHAPE.AREA, -SHAPE.LEN) %>%
  janitor::clean_names()


# furbearer zones ---------------------------------------------------------

furbearer_zones <-
  st_read(here::here('data-raw/Zones.shp')) %>%
  st_transform(., 4326) %>%
  select(-OBJECTID, -Shape_Leng, -Shape_Area) %>%
  janitor::clean_names()


# wolf zones --------------------------------------------------------------

wolf_zones <-
  get_spatial_layer('https://dnrmaps.wi.gov/arcgis/rest/services/DW_Map_Dynamic/EN_Hunting_Zones_WTM_Ext/MapServer/0') %>%
  select(-OBJECTID, -SHAPE.AREA, -SHAPE.LEN) %>%
  janitor::clean_names()


# ruffed grouse -----------------------------------------------------------


# save raw spatial data ---------------------------------------------------

sswids_spatial_layers <-
  list(
    dmus = dmus,
    counties = counties,
    ecological_landscapes = ecological_landscapes,
    turkey_mgt_zones = turkey_mgt_zones,
    furbearer_zones = furbearer_zones,
    wolf_zones = wolf_zones
  )

# this updates the /data folder
use_data(sswids_spatial_layers, overwrite = TRUE)
