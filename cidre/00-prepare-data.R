
 
# 1. Load packages -------------------------------------------------------

library(pacman)

p_load(
  forestdata, geodata, giscoR, janitor, medfateland, climaemet,
  meteospain, sf, silviculture, tidyverse
)
source("R/utils.R")
 
# 2. Load data -----------------------------------------------------------

## Codes
codes_tbl <- bind_rows(
  read.csv2("cidre/data/tree_codes_ifn4.csv"),
  read.csv2("cidre/data/shrub_codes_ifn4.csv")
) |> 
  select(IFNCODE, IFNNAME) |> 
  as_tibble()

## Tenerife area
tenerife_sf <- gisco_get_nuts(
  resolution = "03",
  country    = "Spain",
  nuts_level = 3
) |> 
  filter(NAME_LATN |> str_detect("Tenerife"))
 
## IFN 4 Tenerife
ifn_list <- fd_inventory_spain("Canarias", ifn = 4, path_metadata = "cidre/data/")

## Transform CRS to match Tenerife
ifn_sf <- st_transform(ifn_list$PCDatosMap_sf, 4326)

## Get Tenerife points
ifn_tenerife_sf <- st_intersection(ifn_sf, tenerife_sf) |> 
  select(Estadillo)

# 3. Prepare data --------------------------------------------------------

## 3.1. Pies mayores --------------------------

## Selected plot
plot_sel <- 1365

## Process pies mayores
ifn_processed_tbl <- ifn_list$PCMayores |> 
  # filter(Estadillo %in% ifn_tenerife_sf$Estadillo) |> 
  filter(Estadillo == plot_sel) |> 
  process_pmayores() |> 
  left_join(
    codes_tbl,
    join_by(species == IFNCODE)
  ) |> 
  select(-species) |> 
  select(plot, species = IFNNAME, everything())

## 3.2. Regeneration --------------------------

regeneration_tbl <- ifn_list$PCRegenera |> 
  filter(Estadillo == plot_sel) |> 
  process_regeneration(codes_tbl)

## 3.3. Shrub ---------------------------------

shrub_tbl <- ifn_list$PCMatorral |> 
  process_matorral(codes_tbl)


## 3.4. Soil data -----------------------------

## Get soil data
soil_sf <- add_soilgrids(
  x      = ifn_tenerife_sf |> filter(Estadillo == plot_sel),
  widths = c(300, 700, 1000)
)


## Extract soil data from SF object
soil_tbl <- soil_sf$soil[[1]]

## 3.5. Climate data -------------------------

## Load all stations
aemet_stations_sf <- aemet_stations(return_sf = TRUE)

## Filter those in Tenerife
tenerife_stations_sf <- st_filter(aemet_stations_sf, tenerife_sf)

## Filter the closest
closest_station_sf <- tenerife_stations_sf |> 
  filter(indicativo == "C422A")

## Date of the plot measures?
date_vec <- ifn_list$PCParcelas |> 
  filter(Estadillo == plot_sel) |> 
  pull(FechaIni)

## Stations options
station_options <- aemet_options(
  resolution = "daily",
  start_date = as_date(date_vec),
  end_date   = as_date(date_vec) + 15,
  stations   = closest_station_sf$indicativo,
  api_key    = climaemet::aemet_show_api_key()
)

## Get data
meteo_tbl <- get_meteo_from('aemet', station_options)

climate_sf <- aemet_daily_clim(
  station   = closest_station_sf,
  start     = as_date(date_vec),
  end       = as_date(date_vec) + 10,
  return_sf = TRUE
)

## 3.6. Sort data and export ----------------

## Add soil to IFN data
ifn_plot_data <- list(
  trees   = ifn_processed_tbl,
  soil    = soil_tbl,
  shrub   = shrub_tbl,
  meteo   = meteo_tbl,
  spatial = ifn_tenerife_sf |> filter(Estadillo == plot_sel)
)

## Export
# write_rds(ifn_processed_soil_tbl, "cidre/data/ifn_processed.rds")
# write_rds(ifn_list$PCMayores, "cidre/data/ifn_non_processed.rds")
# write_rds(ifn_tenerife_sf, "cidre/data/ifn_spatial.rds")
write_rds(ifn_plot_data, "cidre/data/ifn_plot_data.rds")



