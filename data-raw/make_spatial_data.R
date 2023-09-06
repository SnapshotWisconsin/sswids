

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

# ruffed grouse -----------------------------------------------------------


# save raw spatial data ---------------------------------------------------

sswids_spatial_layers <-
  list(
    dmus,
    counties
  )

use_data(sswids_spatial_layers, overwrite = TRUE)


load_all()
