

# 1. Load packages -------------------------------------------------------

library(pacman)
p_load(medfate, medfateland, sf, tidyverse)

# 2. Load data -----------------------------------------------------------

## Read processed data
ifn_plot_data <- read_rds("cidre/data/ifn_plot_data.rds")

# 3. Process data --------------------------------------------------------

## 3.1. Create forest object -------------------

## Create empty forest
data_forest <- emptyforest()

## Add tree data to forest object
data_forest$treeData <- ifn_plot_data$trees |> 
    forest_mapTreeTable(
        mapping_x   = c(
          "Species" = "species", 
          "DBH"     = "dclass",
          "Height"  = "h_mean",
          "N"       = "n_ha"
        ),
        SpParams    = SpParamsMED
    )

## Add shrub data to forest object
data_forest$shrubData <- ifn_plot_data$shrub |> 
  forest_mapShrubTable(
    mapping_y = c(
      "Species" = "species",
      "Height" = "H_m",
      "Cover"  = "fcover"
    ),
    SpParams = SpParamsMED
  )

## Summary of forest object
summary(data_forest, SpParamsMED)

## 3.2. Stand-based params -------------------

## LAI
stand_LAI(data_forest, SpParamsMED)

## Mean height
stand_meanTreeHeight(data_forest)

## Foliar biomass
stand_foliarBiomass(data_forest, SpParamsMED)

## Number of trees/ha
stand_treeDensity(data_forest)

## 3.3. Profiles -----------------------------

## Leaf area density
vprofile_leafAreaDensity(data_forest, SpParamsMED)

## Distribution of roots
vprofile_fuelBulkDensity(data_forest, SpParamsMED)


