# =============================================================================
# SEADITO - Pan-European MSEV Demonstrator, single file app
# =============================================================================
# Strategy: load pre-cleaned, pre-joined RDS files written by prepare_data.R.
# Zero geometry cleaning or spatial joins at startup.
#
# PREREQUISITE: run prepare_data.R once before launching, from the project base
# directory (not from Scripts/), so the caches land in Cache/.
#
# This file replaces the app.R + Scripts/ui.R pair. The UI below is the former
# Scripts/ui.R with the OWE driver controls and plots added, so Scripts/ui.R is
# no longer sourced.
# =============================================================================


#______________________________________________________________________________
## Libraries ####
#______________________________________________________________________________

library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(ggrepel)
library(stringr)
library(forcats)
library(tidyr)
library(scales)
library(pals)
library(patchwork)
library(DT)
library(raster)          # for SDM .asc rasters
library(RColorBrewer)
if (requireNamespace("ragg", quietly = TRUE)) options(shiny.useragg = TRUE)


#______________________________________________________________________________
## Helper: keep only polygonal geometry ####
#______________________________________________________________________________
# st_make_valid() can return a GEOMETRYCOLLECTION when it repairs a polygon
# that contains a stray line or point. leaflet's addPolygons cannot draw those
# and crashes on them, so the polygonal parts are extracted here once at load
# time and anything else is dropped.

polygons_only <- function(x) {
  x <- x[!sf::st_is_empty(sf::st_geometry(x)), ]

  gc <- sf::st_geometry_type(x) == "GEOMETRYCOLLECTION"
  if (any(gc)) {
    sf::st_geometry(x)[gc] <- sf::st_collection_extract(
      sf::st_geometry(x)[gc], "POLYGON"
    )
  }

  x[sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"), ]
}


#______________________________________________________________________________
## Load cached GIS data ####
#______________________________________________________________________________

message("Loading cached GIS data...")
coastal_nuts3_area <- readRDS("Cache/coastal_nuts3_area.rds")
coastal_centroids  <- readRDS("Cache/coastal_centroids.rds")
nuts_with_msfd     <- readRDS("Cache/nuts_with_msfd.rds")

# UK and Turkey are outside the demonstrator. Centroids are filtered with the
# same index so they stay row-aligned with the regions.
keep_regions       <- !substr(coastal_nuts3_area$nuts_code, 1, 2) %in% c("UK", "TR")
coastal_nuts3_area <- polygons_only(coastal_nuts3_area[keep_regions, ])
coastal_centroids  <- coastal_centroids[keep_regions, ]
message("  GIS data loaded.")


#______________________________________________________________________________
## Load cached tabular data ####
#______________________________________________________________________________

message("Loading cached tabular data...")
msev_2100_global              <- readRDS("Cache/msev_2100_global.rds")
ac_global                     <- readRDS("Cache/ac_global.rds")
msev_results_rounded          <- readRDS("Cache/msev_results_rounded.rds")
community_risk_rounded        <- readRDS("Cache/community_risk_rounded.rds")
community_sensitivity_rounded <- readRDS("Cache/community_sensitivity_rounded.rds")

# Fisheries ecological risk is read as ecological_risk
if (!"ecological_risk" %in% names(msev_2100_global) &&
    "eco_risk" %in% names(msev_2100_global))
  msev_2100_global <- msev_2100_global %>% rename(ecological_risk = eco_risk)

# Only components present in the cache are offered, so a missing column hides
# its option instead of crashing the map.
fish_components <- c(
  "Ecological Risk"     = "ecological_risk",
  "Community Risk"      = "community_risk",
  "Adaptative Capacity" = "adaptative_capacity",
  "MSEV"                = "msev"
)
fish_components <- fish_components[fish_components %in% names(msev_2100_global)]
message("  Tabular data loaded.")


#______________________________________________________________________________
## Load cached OWE scenario data ####
#______________________________________________________________________________

message("Loading cached OWE scenario data...")
owe_msev_long    <- readRDS("Cache/owe_msev_long.rds")
owe_msev_range   <- readRDS("Cache/owe_msev_range.rds")
owe_by_subregion <- readRDS("Cache/owe_by_subregion.rds")
owe_geom_by_run  <- polygons_only(readRDS("Cache/owe_geom_by_run.rds"))
message("  OWE data loaded.")


#______________________________________________________________________________
## Adaptative capacity: display orientation ####
#______________________________________________________________________________
# Adaptative capacity is stored, and enters MSEV, in vulnerability orientation
# (100 = lowest capacity). For display only it is flipped so that 100 reads as
# the highest capacity. MSEV and every other component are unchanged, and the
# Raw Data tab keeps the stored orientation (owe_msev_long_raw).
#
# adaptive_strength (x axis of the fisheries scatter) is flipped on the
# assumption it shares the stored orientation. If it is already
# strength-oriented, drop it from ac_display_cols_fish.

ac_scale_max <- 100
invert_ac    <- function(x) ac_scale_max - as.numeric(x)

ac_display_cols_fish <- c("adaptative_capacity", "adaptive_strength")
ac_display_cols_owe  <- c("adaptative_capacity")

owe_msev_long_raw <- owe_msev_long

msev_2100_global <- msev_2100_global %>%
  mutate(across(any_of(ac_display_cols_fish), invert_ac))
owe_msev_long <- owe_msev_long %>%
  mutate(across(any_of(ac_display_cols_owe), invert_ac))

ac_legend_note <- "(100 = highest)"


#______________________________________________________________________________
## SDM species configuration ####
#______________________________________________________________________________
# The 14 commercial species of the fisheries analysis. Only species with a
# projection file in the SDM folder are offered on the map; the rest are
# listed in the Data Sources tab as not yet projected.

sdm_dir <- "Data/SDM/2100"

econo_species <- c(
  "Clupea_harengus", "Engraulis_encrasicolus", "Gadus_morhua",
  "Limanda_limanda", "Melanogrammus_aeglefinus", "Merluccius_merluccius",
  "Pleuronectes_platessa", "Salmo_salar", "Salmo_trutta", "Scomber_scombrus",
  "Sprattus_sprattus", "Thunnus_thynnus", "Trisopterus_esmarkii", "Xiphias_gladius"
)

sdm_species_files   <- setNames(paste0(econo_species, "_projection.asc"), econo_species)
sdm_available       <- file.exists(file.path(sdm_dir, sdm_species_files))
sdm_species_choices <- setNames(econo_species[sdm_available],
                                gsub("_", " ", econo_species[sdm_available]))

# Cache loaded rasters in a named list, lazily on first request
sdm_cache <- list()

load_sdm <- function(species_key) {
  if (!is.null(sdm_cache[[species_key]])) return(sdm_cache[[species_key]])
  fpath <- file.path(sdm_dir, sdm_species_files[[species_key]])
  if (!file.exists(fpath)) {
    message("  SDM file not found: ", fpath)
    return(NULL)
  }
  r <- raster::raster(fpath)
  sdm_cache[[species_key]] <<- r
  r
}


#______________________________________________________________________________
## Shared plot theme: transparent background ####
#______________________________________________________________________________
# Added to every plot so the floating panel shows through. The renderPlot
# calls also pass bg = "transparent" so the device itself has no fill.

transparent_theme <- theme(
  plot.background       = element_rect(fill = "transparent", colour = NA),
  panel.background      = element_rect(fill = "transparent", colour = NA),
  legend.background     = element_rect(fill = "transparent", colour = NA),
  legend.box.background = element_rect(fill = "transparent", colour = NA),
  legend.key            = element_rect(fill = "transparent", colour = NA)
)


#______________________________________________________________________________
## Shared plot settings: side plot size, legends, point cap ####
#______________________________________________________________________________
# Every plot in the analysis panel uses this height, so they all keep the same
# rectangular box. Scatter legends run across in rows of four. Scatters show
# at most scatter_top_n regions, those with the highest MSEV.

side_plot_height <- "300px"
scatter_top_n    <- 25

legend_in_rows <- function(n) {
  guide_legend(ncol = min(max(n, 1), 4), byrow = TRUE,
               override.aes = list(size = 2.5))
}


#______________________________________________________________________________
## Basemap ####
#______________________________________________________________________________
# Greyscale, so it never competes with the map colours. Used by the Map
# Explorer and the Data Sources map.

basemap_provider <- "Esri.WorldGrayCanvas"


#______________________________________________________________________________
## Map colours ####
#______________________________________________________________________________
# MSEV uses viridis. Every component uses a single hue from light to dark, and
# dark is always more vulnerable. Adaptative capacity is displayed with
# 100 = highest capacity, so its ramp is reversed: low capacity is dark.
# Set msev_reverse to TRUE to make high MSEV dark as well.

msev_reverse <- FALSE

map_palettes <- list(
  msev                  = list(palette = "viridis", reverse = msev_reverse),
  ecological_risk       = list(palette = "Greens",  reverse = FALSE),
  eco_risk              = list(palette = "Greens",  reverse = FALSE),
  community_risk        = list(palette = "Reds",    reverse = FALSE),
  comm_risk             = list(palette = "Reds",    reverse = FALSE),
  community_sensitivity = list(palette = "Oranges", reverse = FALSE),
  comm_sen              = list(palette = "Oranges", reverse = FALSE),
  adaptative_capacity   = list(palette = "Purples", reverse = TRUE)
)

# Brewer ramps start at their 3rd of 9 steps, so no colour gets close to
# white and every region stays visible on the grey basemap.
seq_cols <- function(palette) RColorBrewer::brewer.pal(9, palette)[3:9]

make_pal <- function(var, values) {
  p    <- map_palettes[[var]]
  cols <- if (p$palette == "viridis") viridisLite::viridis(256) else seq_cols(p$palette)
  colorNumeric(cols, domain = values, reverse = p$reverse, na.color = "#d0d0d0")
}

# SDM suitability overlay: golden mustard, light to dark
sdm_cols <- c("#f3e3a1", "#e8cc6a", "#dbb43a", "#c99a1e",
              "#b08414", "#8f6a0f", "#6b4f0a")


#______________________________________________________________________________
## Terminology: hover info icons ####
#______________________________________________________________________________

info_icon <- function(text) {
  tags$span(class = "info-i", title = text, icon("circle-info"))
}

label_with_info <- function(label, text) tagList(label, info_icon(text))

glossary <- list(
  driver = paste(
    "The pressure of change assessed for each coastal NUTS3 region.",
    "Fisheries and Climate Change: how projected shifts in habitat suitability of 14 commercial fish species by 2100 (RCP8.5) affect the coastal communities that depend on them.",
    "OWE Development: how offshore wind farms, deployed under three stakeholder scenarios up to 2050, affect the surrounding ecosystems and communities.",
    "Data Gaps: which regions are missing values in each dataset.",
    sep = "\n\n"
  ),
  component = "One dimension of Marine Socio-Ecological Vulnerability (MSEV), mapped per coastal NUTS3 region. MSEV is shown in viridis; each component has its own colour, and darker is always more vulnerable. Hover the i next to each option for what it measures.",
  scenario = paste(
    "OWE deployment scenarios, chosen by the SEADITO team and validated with stakeholders in workshops. Each reaches its final number of farms by 2050, deploying the most suitable planned sites first, in line with national MSP plans and the TEN-E Regulation.",
    "BAU: Business as Usual (OCSP0), current planning carries on.",
    "SUS: Blue Road (OCSP1), sustainability-led deployment.",
    "Growth: Endless Growth (OCSP3), the largest build-out, with the strongest effects on ecosystems.",
    sep = "\n\n"
  ),
  time = paste(
    "Production: OWE farms already in production. One baseline run, the same under every scenario.",
    "2040 and 2050: farms deployed by that year under the selected scenario. Planned sites carry high deployment uncertainty, so read these as plausible futures rather than forecasts.",
    sep = "\n\n"
  ),
  sdm  = "Projected environmental suitability for 2100 under RCP8.5, from MaxEnt species distribution models built on the OCLE environmental database (de la Hoz et al. 2025). 0 = unsuitable, 1 = highly suitable; darker mustard is more suitable. These projections feed the fisheries Ecological Risk.",
  gaps = paste(
    "Each coastal NUTS3 region is checked component by component.",
    "Complete (yellow): every component present.",
    "Partial gaps (orange): some components missing; hover a region to see which.",
    "No data (dark brown): the region is absent from the dataset.",
    "No development (grey, OWE only): no project in any run, so missing values are expected rather than a gap.",
    sep = "\n"
  )
)

# Component definitions differ between drivers, so each has its own set,
# keyed by the column name.
glossary_fish <- c(
  msev                = "Marine Socio-Ecological Vulnerability to climate-driven change in commercial fisheries by 2100 (RCP8.5). Combines Ecological Risk, Community Risk and Adaptative Capacity into one score per coastal NUTS3 region. Higher = more vulnerable.",
  ecological_risk     = "Projected change in habitat suitability by 2100 (RCP8.5) for the 14 commercial species, such as cod, hake, herring, mackerel, anchovy, sprat and bluefin tuna, from species distribution models built on OCLE data. Darker = higher risk to the fishery resource.",
  community_risk      = "Risk to the coastal communities that rely on these fisheries: their exposure to the projected change in the resource combined with their sensitivity to it, such as economic dependence on the affected species. Darker = higher risk.",
  adaptative_capacity = "Ability of a region to anticipate, cope with and recover from change, organised by the livelihood capitals of Metcalf et al. (2015). Nine indicators: GDP, employment, population and foreign-born population (NUTS3), plus population in agriculture, forestry and fishing, high-tech employment, population change, tertiary education and tourist nights (Eurostat NUTS2, assigned to each NUTS3). Indicators are min-max normalised within each country, or within the MSFD subregion where a country has no spread. Shown so that 100 = highest capacity; darker = lower capacity, so more vulnerable."
)

glossary_owe <- c(
  msev                = "Marine Socio-Ecological Vulnerability to offshore wind development. Combines Ecological Risk, Community Risk and Adaptative Capacity with a geometric mean. Every input is a share of a fixed regional total, so adding projects can never lower a region's score and all runs are directly comparable. Higher = more vulnerable.",
  eco_risk            = "Pressure on marine ecosystems around the farms: species suitability and habitat sensitivity scores combined with a geometric mean, scaled by the share of the region's sea inside a 10 km ecological zone around the projects. Darker = higher risk.",
  comm_risk           = "Exposure of coastal communities to the farms, as shares of fixed regional totals, such as the share of the population within 40 km of the coast that lives inside the 40 km social buffer of the projects. Darker = higher risk.",
  comm_sen            = "How sensitive the region's people and sea space are to added development, as shares of fixed regional totals, such as the coastline facing the projects and how much of the regional sea is already protected (MPAs within 10 km) or developed. Darker = more sensitive.",
  adaptative_capacity = "Ability of a region to anticipate, cope with and recover from change, from socio-economic indicators grouped by the livelihood capitals of Metcalf et al. (2015). Shown so that 100 = highest capacity; darker = lower capacity, so more vulnerable."
)

# Radio button labels, each with its own info icon
component_choice_names <- function(components, texts) {
  lapply(names(components), function(lab) {
    label_with_info(lab, texts[[components[[lab]]]])
  })
}


#______________________________________________________________________________
## OWE driver constants ####
#______________________________________________________________________________

# Number of bars drawn in the zoom-dependent plots. Change these three only.
owe_range_top_n   <- 5
owe_country_top_n <- 5
owe_movers_top_n  <- 25

# Scenario order used everywhere: menus, legends, bars and tables
owe_scenarios <- c("BAU", "SUS", "Growth")
owe_times     <- c("Production", "2040", "2050")

owe_variables <- c(
  "MSEV"                  = "msev",
  "Ecological Risk"       = "eco_risk",
  "Community Risk"        = "comm_risk",
  "Community Sensitivity" = "comm_sen",
  "Adaptative Capacity"   = "adaptative_capacity"
)

# Accent, so the three scenarios stay distinguishable in the plots
scenario_cols <- setNames(RColorBrewer::brewer.pal(8, "Accent")[1:3],
                          owe_scenarios)

# Project footprints on the map are coloured by their STATUS
# If owe_geom_by_run.rds was built before STATUS was carried, every project
# gets one class so the map still draws.
if (!"STATUS" %in% names(owe_geom_by_run))
  owe_geom_by_run$STATUS <- "Selected project"

# Black, brown and bright neon hues, none of which appear in the viridis or
# single-hue map palettes. Recycled if there are more statuses than colours.
status_cols   <- c("#000000", "#00e5ff", "#ff1493", "#8b4513", "#000080")
status_levels <- sort(unique(owe_geom_by_run$STATUS))
status_pal    <- colorFactor(rep_len(status_cols, length(status_levels)),
                             levels = status_levels, na.color = "#808080")

owe_side_plots <- c(
  "MSEV range (top 5)"                           = "range",
  "Projects and area by country (top 5)"         = "country",
  "Ecological Risk vs Community Risk"            = "eco_comm",
  "Community Sensitivity vs Adaptative Capacity" = "sen_ac"
)

owe_analysis_info <- paste(
  "MSEV range (top 5): the 5 regions in view whose MSEV changes most over time. For each scenario the bar is the highest minus the lowest MSEV across Production, 2040 and 2050, so long bars flag the regions most affected by the build-out.",
  "Projects and area by country (top 5): number of OWE projects and their total area in the regions in view, for the 5 countries with most projects, by time step under the selected scenario. The number is the value; the shading is its share of the largest value across all runs in view.",
  "Ecological Risk vs Community Risk: one point per region in view for the selected run, coloured by MSFD subregion. Regions top right face both high ecological and high community risk. At most 25 regions, those with the highest MSEV.",
  "Community Sensitivity vs Adaptative Capacity: as above. Regions top left combine high sensitivity with low capacity, so are the least able to absorb the change. Adaptative Capacity is shown so that 100 = highest capacity.",
  sep = "\n\n"
)

owe_movers_info <- "How each region's MSEV shifts between runs: the Production baseline and each scenario at 2040 and 2050. Each point is one run, sized by the number of OWE projects the region carries in that run. A wide spread means the outcome for that region depends strongly on which scenario plays out. Shows the 25 regions in view with the widest spread."


#______________________________________________________________________________
## Fisheries driver constants ####
#______________________________________________________________________________

# Regions shown in the MSEV bar plot and the adaptative capacity components plot
fish_msev_top_n <- 5
ac_top_n        <- 5

fish_side_plots <- c(
  "MSEV (top 5)"                                 = "msev_top",
  "Ecological Risk vs Community Risk"            = "eco_comm",
  "Community Sensitivity vs Adaptative Capacity" = "sen_ac"
)

fish_analysis_info <- paste(
  "MSEV (top 5): the 5 regions in view with the highest MSEV in the 2100 scenario, the most vulnerable to climate-driven change in commercial fisheries.",
  "Ecological Risk vs Community Risk: one point per region in view, coloured by MSFD subregion. Regions top right face both high ecological and high community risk. At most 25 regions, those with the highest MSEV.",
  "Community Sensitivity vs Adaptative Capacity: as above. Regions top left combine high sensitivity with low capacity, so are the least able to absorb the change. Adaptative Capacity is shown so that 100 = highest capacity.",
  sep = "\n\n"
)

ac_components_info <- "Livelihood capital scores (Metcalf et al. 2015) behind Adaptative Capacity, for the 5 regions in view with the lowest average capacity. Each panel is one capital. Shown so that the top of the scale is the highest capacity, so short bars point to the capitals holding a region back."

# Livelihood capital scores behind Adaptative Capacity (Metcalf et al. 2015),
# plotted beneath the map. Read from the fisheries adaptative capacity output:
# 0 to 1, high = poor capital. Physical capital is not represented.
load("Output/Fisheries/adaptative_capacity_nuts3.RData")

ac_capitals <- c(
  "Financial\n(GDP pc, tourist nights pc)"      = "capital_financial",
  "Human\n(employment, high-tech, tertiary)"     = "capital_human",
  "Social\n(population, increase, foreign-born)" = "capital_social",
  "Natural\n(agriculture, forestry, fishing)"    = "capital_natural"
)

ac_capital_scores <- adaptative_capacity_nuts3 %>%
  dplyr::select(nuts_code, name, all_of(unname(ac_capitals)))


#______________________________________________________________________________
## Data gaps driver ####
#______________________________________________________________________________
# For each dataset, every coastal region is checked component by component.
#   Complete       every component present
#   Partial gaps   some components missing
#   No data        region absent from the dataset, or every component missing
#   No development OWE only: the region carries no project in any run, so a
#                  missing value there is expected rather than a gap

gap_region_codes <- coastal_nuts3_area$nuts_code

ac_gap_vars <- c(
  "Population"   = "pop_2021",
  "Employment"   = "employment_2021",
  "Foreign-Born" = "foreign_born_population",
  "GDP"          = "gdp_2021"
)
ac_gap_vars  <- ac_gap_vars[ac_gap_vars %in% names(ac_global)]
owe_gap_vars <- owe_variables[owe_variables %in% names(owe_msev_long)]

build_gaps <- function(df, vars, absent_status) {
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)

  flags <- df %>%
    dplyr::select(nuts_code, all_of(unname(vars))) %>%
    summarise(across(everything(), ~ any(is.na(.x))), .by = nuts_code) %>%
    pivot_longer(-nuts_code, names_to = "var", values_to = "missing") %>%
    mutate(var = names(vars)[match(var, vars)])

  # Regions absent from a dataset count as missing every component
  if (absent_status == "No data")
    flags <- flags %>%
      tidyr::complete(nuts_code = gap_region_codes, var = names(vars),
                      fill = list(missing = TRUE))

  flags <- flags %>% filter(nuts_code %in% gap_region_codes)

  status <- flags %>%
    summarise(
      n_missing    = sum(missing),
      n_total      = n(),
      missing_vars = paste(var[missing], collapse = ", "),
      .by = nuts_code
    ) %>%
    mutate(status = case_when(
      n_missing == 0       ~ "Complete",
      n_missing == n_total ~ "No data",
      TRUE                 ~ "Partial gaps"
    ))

  status <- tibble::tibble(nuts_code = gap_region_codes) %>%
    left_join(status, by = "nuts_code") %>%
    mutate(status = coalesce(status, absent_status))

  list(flags = flags, status = status, vars = names(vars))
}

gap_datasets <- list(
  fisheries = build_gaps(msev_2100_global, fish_components, "No data"),
  ac        = build_gaps(ac_global, ac_gap_vars, "No data"),
  owe       = build_gaps(owe_msev_long %>% filter(n_projects > 0),
                         owe_gap_vars, "No development")
)

gap_choices <- c(
  "Fisheries and Climate Change"   = "fisheries",
  "Adaptative Capacity indicators" = "ac",
  "OWE Development"                = "owe"
)

# Single hue, light to dark as the gap gets bigger, starting well away from
# white. Not applicable is grey.
gap_levels <- c("Complete", "Partial gaps", "No data", "No development")
gap_pal    <- colorFactor(c("#fec44f", "#ec7014", "#8c2d04", "#bdbdbd"),
                          levels = gap_levels, ordered = TRUE)


#______________________________________________________________________________
## Raw data sources ####
#______________________________________________________________________________
# One row per raw dataset and its provider. Edit this table directly to
# correct a provider or add a dataset; "To confirm" marks providers still to
# be checked. Spatial unit is either Raster or Shapefile. The coverage column
# is not shown; it tells the Data Sources map what to draw: nuts3, owe_geom,
# ac:<column in ac_global>, sdm:<species>, or NA when the dataset is not
# cached in the app.

raw_sources <- tibble::tribble(
  ~Dataset, ~Provider, ~Description, ~`Spatial unit`, ~`Used in`, ~coverage,
  "NUTS3 regions", "Eurostat GISCO", "Region boundaries and codes, coastal regions selected", "Shapefile", "All drivers", "nuts3",
  "MSFD marine regions and subregions", "EEA", "Marine region and subregion boundaries", "Shapefile", "All drivers", NA,
  "Coastline", "Derived from NUTS3 and MSFD boundaries", "Coastline length per region: NUTS3 boundaries intersected with a 400 m buffer of the MSFD marine boundary", "Shapefile", "OWE Development", "nuts3",
  "Population", "ARDECO", "Total population", "Shapefile", "Fisheries: Adaptative Capacity", "ac:pop_2021",
  "Employment", "ARDECO", "Total employment", "Shapefile", "Fisheries: Adaptative Capacity", "ac:employment_2021",
  "GDP", "ARDECO", "Gross domestic product", "Shapefile", "Fisheries: Adaptative Capacity", "ac:gdp_2021",
  "Foreign-born population", "To confirm", "Foreign-born residents", "Shapefile", "Fisheries: Adaptative Capacity", "ac:foreign_born_population",
  "Population in agriculture, forestry and fishing", "Eurostat", "Population in agriculture, forestry and fishing", "Shapefile", "Fisheries: Adaptative Capacity", NA,
  "Employment in high-tech sectors", "Eurostat (TGS00039)", "Employment in high-technology sectors", "Shapefile", "Fisheries: Adaptative Capacity", NA,
  "Population change", "Eurostat (TGS00099)", "Population change", "Shapefile", "Fisheries: Adaptative Capacity", NA,
  "Tertiary educational attainment", "Eurostat (TGS00109)", "Population with tertiary education", "Shapefile", "Fisheries: Adaptative Capacity", NA,
  "Nights spent at tourist accommodation", "Eurostat (TGS00111)", "Tourist nights", "Shapefile", "Fisheries: Adaptative Capacity", NA,
  "OWE project areas", "EMODnet Human Activities", "Offshore wind farm footprints and status", "Shapefile", "OWE Development", "owe_geom",
  "Marine protected areas", "To confirm", "MPA polygons, dissolved", "Shapefile", "OWE Development", NA,
  "Population grid", "To confirm", "Gridded population, for population within 40 km of the coast and in OWE buffers", "Raster", "OWE Development", NA,
  "Habitat sensitivity layers", "To confirm", "Habitat layers used for sensitivity scoring", "Shapefile", "OWE Development: Ecological Risk", NA
)

# One row per commercial species. Species without a projection file yet are
# still listed, with no coverage to draw.
raw_sources <- dplyr::bind_rows(raw_sources, tibble::tibble(
  Dataset        = paste0("Species distribution model: ", gsub("_", " ", econo_species)),
  Provider       = "OCLE",
  Description    = ifelse(sdm_available,
                          "MaxEnt projection of environmental suitability, 0 to 1, 2100 (RCP8.5)",
                          "Projection not yet in the SDM folder"),
  `Spatial unit` = "Raster",
  `Used in`      = "Fisheries: Ecological Risk",
  coverage       = ifelse(sdm_available, paste0("sdm:", econo_species), NA_character_)
))

source_provider_choices <- c("All", sort(unique(raw_sources$Provider)))
source_used_in_choices  <- c("All", sort(unique(raw_sources$`Used in`)))


#______________________________________________________________________________
## Scenario card content ####
#______________________________________________________________________________
# Content transcribed from the SEADITO scenario card document.
# Anything left as "tbd" in the source is kept as-is.

scenario_card <- list(
  "General Information" = tibble::tribble(
    ~question, ~answer,
    "Case study",
    "PAN EU",
    "Scenario number",
    "Three levels of OWE occupation, each tied to one scenario: Business as Usual (OCSP0), Blue Road (OCSP1), and Endless Growth (OCSP3).",
    "Scenario type",
    "Exploratory scenario: for climate change a quantitative approach with IPCC models was used.\n\nExploratory / policy scenario: for OWE placement a percentage of the projected sites was linked to suitability to represent a gradual deployment. Sectorial policy was taken into account, such as national MSP plans and the TEN-E Regulation.",
    "Case study areas",
    "PAN EU. EEZ down to 2000 m depth.",
    "Time horizon",
    "Climate change models applied at 2050 and 2100.\n\nFor OWE, the number of developments (planned areas) grows from 2030 to 2050, linked to site suitability.",
    "Source of the scenario",
    "Chosen by SEADITO team expert criteria and validated with stakeholders during workshop sessions."
  ),
  "Scenario Storyline" = tibble::tribble(
    ~question, ~answer,
    "Challenges addressed",
    "Ecological: changes in habitat suitability due to climate change and the effect on coastal communities depending on commercial fishery resources.\n\nOWE: the effects of gradual deployment of different projects. Based on area suitability the number of deployments is modulated, showing effects on communities under different OWE scenarios and their interaction with ecological and social components.",
    "Short narrative",
    "Climate change will affect coastal communities and their dependence on marine ecological resources. For fisheries, species suitability shifts in distribution, improving fishing in some areas and worsening it in others, depending on geographical distribution, the specific species, and community economic reliance.\n\nFor OWE, based on the stakeholder workshop, each scenario reaches a final deployment number of OWE farms according to the OCSP scenarios. Deployment is prioritised by regional suitability, most suitable areas first. The total per scenario is shown as completed at the 2050 time stamp, with effects on surrounding communities and ecosystems observed through the MSEV framework.",
    "Key assumptions",
    "IPCC RCP8.5 climate projections for the fisheries scenarios.\n\nFor OWE, the required policy is implementation of the already projected sites. Planned areas carry high deployment uncertainty but are key triggers for an MSEV analysis.",
    "Relation to other scenarios",
    "The Endless Growth scenario changed during discussion: the effects on the ecosystem became more drastic."
  ),
  "Geospatial Boundary Conditions" = tibble::tribble(
    ~question, ~answer,
    "Spatial extent",
    "Complete Pan EU EEZ up to 2000 m depth.",
    "Spatial units",
    "Complete Pan EU EEZ up to 2000 m depth.",
    "Spatial resolution",
    "Per NUTS3 region.",
    "Key spatial constraints and limitations",
    "tbd"
  ),
  "Key Driving Forces & Cascading Effects" = tibble::tribble(
    ~question, ~answer,
    "Driving forces",
    "Human drivers, either through the effects of climate change or through differences in OWE farm deployment.",
    "Causal chains",
    "Climate change drives changes in species distribution through effects on environmental suitability."
  ),
  "Response Measures" = tibble::tribble(
    ~question, ~answer,
    "Response measures",
    "Identified during the SEADITO workshop by driver of change:\n\nClimate change:\n- Nature-based solutions\n- Regional assistance mechanisms for coastal communities\n- MPA expansion and regulatory enforcement\n- Decarbonising shipping and fishery vessels (green shipping)\n\nOffshore wind energy deployment:\n- Nature-inclusive designs\n- Regional assistance mechanisms for coastal communities\n- Research on co-existence mechanisms with other human uses"
  )
)


#______________________________________________________________________________
## Helper: render one scenario card section ####
#______________________________________________________________________________

render_card_section <- function(section_title, df) {
  header_row <- tags$tr(
    tags$td(
      colspan = 2,
      style = "padding:9px 12px;font-size:13px;font-weight:700;
               letter-spacing:0.03em;text-transform:uppercase;
               color:#fff;background:#34667d;",
      section_title
    )
  )

  body_rows <- lapply(seq_len(nrow(df)), function(i) {
    tags$tr(
      tags$td(
        style = "width:26%;vertical-align:top;padding:8px 12px;
                 font-weight:600;color:#2c3e50;background:#f7f9fb;
                 border-bottom:1px solid #e3e8ee;",
        df$question[i]
      ),
      tags$td(
        style = "vertical-align:top;padding:8px 12px;
                 border-bottom:1px solid #e3e8ee;",
        HTML(gsub("\n", "<br>", df$answer[i]))
      )
    )
  })

  tagList(header_row, body_rows)
}


# =============================================================================
# UI
# =============================================================================

ui <- tagList(

  #____________________________________________________________________________
  ## Global CSS ####
  #____________________________________________________________________________

  tags$head(
    tags$style(HTML("
      .navbar {
        background-color: #07284D !important;
        min-height: 80px;
        position: relative;
      }
      .navbar-header {
        width: auto;
        position: relative;
      }
      .navbar-brand {
        padding: 15px 0px;
        height: 80px;
        display: flex;
        align-items: center;
        float: left;
      }
      .navbar-brand img {
        max-height: 50px;
        width: auto;
      }

      /* Funding logo, bottom right after all page content */
      .footer-logo {
        text-align: right;
        padding: 10px 15px 20px 15px;
      }
      .footer-logo img {
        height: 80px;
        width: auto;
      }

      .navbar-nav > li > a {
        padding-top: 30px;
        padding-bottom: 30px;
        line-height: 20px;
      }

      .navbar-brand, .navbar-nav > li > a {
        color: #ffffff !important;
      }
      .navbar-nav > .active > a {
        background-color: #245580 !important;
      }

      #logo-bottom-left {
        position: fixed;
        bottom: 10px;
        left: 10px;
        z-index: 1000;
      }
      #logo-bottom-left img {
        height: 80px;
        width: auto;
        background: white;
        padding: 5px;
        border-radius: 4px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.25);
      }

      .intro-body {
        max-width: 900px; margin: 30px auto;
        font-size: 15px; line-height: 1.7;
      }
      .intro-body h2 { color: #07284D; margin-top: 30px; }
      .intro-body h3 { color: #07284D; margin-top: 20px; }
      .intro-body .ref-list { font-size: 13px; }

      .well { background: #f5f8fb; border: 1px solid #d0dce8; }

      /* Map Explorer: full-width map with floating panels */
      .map-wrap {
        position: relative;
      }
      .float-panel {
        position: absolute;
        z-index: 1001;
        background: rgba(255, 255, 255, 0.82);
        backdrop-filter: blur(3px);
        border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.25);
        padding: 10px 12px;
      }
      /* Left panel: no overflow clipping, so selectize dropdowns can open
         outside the box */
      .float-left {
        top: 10px;
        left: 55px;              /* clears the leaflet zoom buttons */
        width: 230px;
      }
      /* Right panel: fixed width so the side plots keep a set rectangular
         shape. Stops short of the bottom so the legends stay visible */
      .float-right {
        top: 10px;
        right: 10px;
        width: 480px;
        max-height: calc(100% - 170px);
        overflow-y: auto;
        overflow-x: hidden;
      }
      .float-title {
        top: 10px;
        left: 50%;
        transform: translateX(-50%);
        padding: 4px 14px;
        font-size: 16px;
        font-weight: 700;
        color: #1a3a5c;
        white-space: nowrap;
        pointer-events: none;
      }
      .float-panel hr { margin: 8px 0; }

      /* Bottom right legends sit side by side, aligned on their bottom
         edge, with the attribution on its own line underneath */
      .leaflet-bottom.leaflet-right {
        display: flex;
        flex-direction: row;
        flex-wrap: wrap;
        justify-content: flex-end;
        align-items: flex-end;
      }
      .leaflet-bottom.leaflet-right .leaflet-control {
        float: none;
        clear: none;
      }
      .leaflet-bottom.leaflet-right .leaflet-control-attribution {
        order: 99;
        flex-basis: 100%;
        text-align: right;
      }
      /* Legends scaled down so they fit under the side plot panel. zoom,
         unlike transform, also shrinks the space they take up */
      .leaflet-bottom.leaflet-right .info.legend {
        zoom: 0.75;
        margin-right: 6px;
        margin-bottom: 6px;
      }

      /* A tooltip that can take the mouse steals the mouseout from the
         polygon underneath it, which is what leaves it stranded. */
      .leaflet-tooltip { pointer-events: none !important; }

      /* Hide/show helpers */
      .hidden { display: none !important; }

      /* Hover info icons for terminology */
      .info-i {
        color: #34667d;
        margin-left: 4px;
        cursor: help;
        font-size: 12px;
      }

      /* Download icon, top right corner of the analysis panel */
      .dl-corner {
        position: absolute;
        top: 8px;
        right: 10px;
        z-index: 2;
        padding: 1px 6px;
      }
      .float-right .shiny-input-container { width: calc(100% - 34px); }
    ")),

    # Leaflet can leave a tooltip behind when the cursor exits a polygon
    # quickly or when the layer under it is redrawn. This tracks whether the
    # cursor is genuinely over a feature and clears anything stranded.
    tags$script(HTML("
      $(document).ready(function() {
        var overFeature = false;

        $(document).on('mouseover', '.leaflet-interactive', function() {
          overFeature = true;
        });

        $(document).on('mouseout', '.leaflet-interactive', function() {
          overFeature = false;
          setTimeout(function() {
            if (!overFeature) { $('.leaflet-tooltip').remove(); }
          }, 80);
        });

        $(document).on('mouseleave', '.leaflet-container', function() {
          overFeature = false;
          $('.leaflet-tooltip').remove();
        });

        // Anything orphaned by a redraw goes once the reactive round ends
        $(document).on('shiny:idle', function() {
          if (!overFeature) { $('.leaflet-tooltip').remove(); }
        });
      });
    "))
  ),

  #____________________________________________________________________________
  ## Navigation ####
  #____________________________________________________________________________

  div(
    style = "position: relative;",
    navbarPage(
      id    = "main_tabs",
      title = div(
        style = "display: flex; align-items: center;",
        tags$img(src = "Seadito_logo_big.png", height = "50px",
                 style = "margin-right:10px;"),
        span("Pan-European Marine Socio-Ecological Vulnerability Demonstrator",
             style = "color:#ffffff; font-size:20px; font-weight:600;")
      ),

      # =====================================================================
      # TAB 1 - INTRODUCTION
      # =====================================================================
      tabPanel(
        "Introduction",
        div(
          class = "intro-body",

          h2("Introduction"),

          h3("Aim of the Pan-European Marine Socio-Ecological Demonstrator"),
          p("The aim of the Pan-European Demonstrator is to provide a tool to explore
            the application of Marine Socio-Ecological Vulnerability (MSEV) to coastal
            and marine environments to different drivers of change at Pan-European scale
            (including major EU sea basins): Mediterranean Sea, Black Sea, Baltic Sea,
            NE Atlantic."),

          h3("What is Marine Socio-Ecological Vulnerability (MSEV)?"),
          p("Socio-ecological vulnerability refers to the extent to which human activities
            and climate change induce environmental degradation that affects the exposure,
            sensitivity, and adaptative capacity of a social-ecological system to anticipate,
            cope with, and recover from hazards (adapted from Depietri 2020)."),
          p("MSEV is based on three components, the ecosystem dimension, the societal
            dimension and the Adaptative Capacity (see Figure 1)."),
          p(em("Note on reading Adaptative Capacity: throughout the Map Explorer,
            Adaptative Capacity is displayed so that 100 is the highest capacity.
            It is stored and combined into MSEV in the opposite orientation
            (100 = lowest capacity), so only the display is flipped and the MSEV
            values are unchanged. The Raw Data tab shows the stored orientation.")),

          div(
            style = "text-align:center; margin:20px 0;",
            tags$img(src = "schematic_represantation.png",
                     style = "max-width:100%; border-radius:6px;
                              box-shadow:0 2px 8px rgba(0,0,0,0.15);"),
            tags$p(style = "font-size:12px; color:#555; margin-top:6px;",
                   "Figure 1. Schematic representation of the components of Marine Socio-Ecological Vulnerability. Author: D. Depellegrin (IHCantabria).")
          ),

          h3("Overview of application examples"),
          p("Marine socio-ecological vulnerability (MSEV) frameworks assess how marine
            ecosystems, coastal communities, and related economies are exposed and
            sensitive to environmental, social, and economic stressors, as well as their
            capacity to adapt. MSEV combines ecological vulnerability (e.g., habitat loss
            or declining fish stocks), social vulnerability (e.g., dependence on fisheries),
            and adaptative capacity (e.g., education, employment, income). When multiple
            stressors interact across time and space, such as climate change, human
            activities, and infrastructure development, cumulative MSEV (CMSEV) emerges,
            amplifying impacts and reducing overall system resilience. These frameworks
            have been widely applied in the Blue Economy, particularly fisheries, to
            assess climate-driven species shifts, competition for marine space, and risks
            to ecosystem services. Applications range from local case studies in fisheries
            and tourism to event-based assessments (e.g., oil spills) and large-scale
            spatial planning tools, including Pan-European vulnerability indices linked
            to policy transitions like the European Green Deal."),

          h3("Policy relevance for coastal management and planning"),
          p("MSEV is highly relevant for Maritime Spatial Planning (MSP) because it
            provides a system-level understanding of who and what is at risk, why,
            and where, enabling planners to make spatial decisions that are environmentally
            sustainable, socially equitable, and resilient to change."),
          tags$ul(
            tags$li("Identifying ecologically sensitive habitats and socially dependent
                     activities in the same spatial units."),
            tags$li("Revealing where economic benefits depend on fragile ecosystems."),
            tags$li("Making trade-offs explicit between development and ecosystem
                     service provision.")
          ),
          p("MSEV can be relevant for the upcoming EU Coastal Communities Strategies because:"),
          tags$ul(
            tags$li("Treats communities and ecosystems as interdependent."),
            tags$li("Recognises that environmental degradation and social vulnerability
                     reinforce each other."),
            tags$li("Moves beyond sectoral thinking to system-level resilience.")
          ),

          h3("Data sources"),
          p("The Pan-European Demonstrator is developed using and extending the following
            data resources and national and Pan-European level:"),
          div(
            style = "text-align:center; margin:20px 0;",
            tags$img(src = "data_sources.png",
                     style = "max-width:100%; border-radius:6px;
                              box-shadow:0 2px 8px rgba(0,0,0,0.15);")
          ),

          h3("Further Reading"),
          div(
            class = "ref-list",
            tags$p(strong("Frameworks, theoretical background and data")),
            tags$ol(
              tags$li("Armoskaite, A., Bonnevie, I.M., Depellegrin, D., Cabanillas, A.G., Bellon, G., Kastanidi, E., Neukirch, M., Von Thenen, M., Sanchez-Jimenez, A., Schroder, L., Socrate, J., Strake, S., Hansen, H.S., 2026. Pathways to social-ecological indicators in the era of Digital Twins of the Earth. Front. Ocean Sustain. 4. https://doi.org/10.3389/focsu.2026.1767068."),
              tags$li("Depietri, Y., 2020. The social-ecological dimension of vulnerability and risk to natural hazards. Sustain Sci 15, 587-604. https://doi.org/10.1007/s11625-019-00710-y."),
              tags$li("Fritzsche, K., Schneiderbauer, S., Buseck, P., Kienberger, S., Buth, M., Zebisch, M. and Kahlenborn, W. (2014). The Vulnerability Sourcebook. Deutsche Gesellschaft fur Internationale Zusammenarbeit (GIZ) GmbH.")
            ),
            tags$p(strong("European cases studies")),
            tags$ol(start = 4,
              tags$li("Fernandes, M. da L., Sanz Larruga, F.J., Alves, F.L., 2022. Spatial characterization of marine socio-ecological systems: A Portuguese case study. Journal of Cleaner Production 363, 132381. https://doi.org/10.1016/j.jclepro.2022.132381."),
              tags$li("Lazzari, N., Becerro, M.A., Sanabria-Fernandez, J.A., Martin-Lopez, B., 2021. Assessing social-ecological vulnerability of coastal systems to fishing and tourism. Science of The Total Environment 784, 147078. https://doi.org/10.1016/j.scitotenv.2021.147078."),
              tags$li("Gomez Murciano, M., Liu, Y., Unal, V., Sanchez LIzaso, J.L., 2021. Comparative analysis of the social vulnerability assessment to climate change applied to fisheries from Spain and Turkey. Sci Rep 11, 13949. https://doi.org/10.1038/s41598-021-93165-0."),
              tags$li("Nelson, J.R., Grubesic, T.H., 2018. Oil spill modeling: Risk, spatial vulnerability, and impact assessment. Progress in Physical Geography 42, 112-127. https://doi.org/10.1177/0309133317744737."),
              tags$li("Rodriguez-Pose, A., Bartalucci, F., 2024. The green transition and its potential territorial discontents. Cambridge Journal of Regions, Economy and Society 17, 339-358. https://doi.org/10.1093/cjres/rsad039.")
            ),
            tags$p(strong("Global case studies")),
            tags$ol(start = 9,
              tags$li("Micheli, F., Saenz-Arroyo, A., Aalto, E., Beas-Luna, R., Boch, C.A., Cardenas, J.C., De Leo, G.A., Diaz, E., Espinoza-Montes, A., Finkbeiner, E., Freiwald, J., Fulton, S., Hernandez, A., Lejbowicz, A., Low, N.H.N., Martinez, R., McCay, B., Monismith, S., Precoma-de la Mora, M., Romero, A., Smith, A., Torre, J., Vazquez-Vera, L., Woodson, C.B., 2024. Social-ecological vulnerability to environmental extremes and adaptation pathways in small-scale fisheries of the southern California Current. Front. Mar. Sci. 11. https://doi.org/10.3389/fmars.2024.1322108."),
              tags$li("Thiault, L., Marshall, P., Gelcich, S., Collin, A., Chlous, F., Claudet, J., 2018. Mapping social-ecological vulnerability to inform local decision making. Conservation Biology 32, 447-456. https://doi.org/10.1111/cobi.12989."),
              tags$li("Koehn, L.E., Nelson, L.K., Samhouri, J.F., Norman, K.C., Jacox, M.G., Cullen, A.C., Fiechter, J., Buil, M.P., Levin, P.S., 2022. Social-ecological vulnerability of fishing communities to climate change: A U.S. West Coast case study. PLOS ONE 17, e0272120. https://doi.org/10.1371/journal.pone.0272120.")
            ),
            tags$p(strong("OCLE - Open Access database on climate change effects on litoral and oceanic ecosystems")),
            tags$ol(start = 1,
              tags$li("de la Hoz, C.F., Ramos, E., Acevedo, A., Menendez, M., Cabanillas, A.G., Puente, A., Juanes, J.A., 2025. OCLE: An updated of the European high-resolution database for assessing marine ecological systems under present conditions and climate change scenarios. Progress in Oceanography 237, 103533. https://doi.org/10.1016/j.pocean.2025.103533."),
              tags$li("de la Hoz, C.F., Ramos, E., Acevedo, A., Puente, A., Losada, I.J., Juanes, J.A., 2018. OCLE: A European open access database on climate change effects on littoral and oceanic ecosystems. Progress in Oceanography 168, 222-231. https://doi.org/10.1016/j.pocean.2018.09.021.")
            )
          ),

          h3("Contact & contribution"),
          tags$ul(
            tags$li(strong("Daniel Depellegrin (Lead):"), " daniel.depellegrin@unican.es"),
            tags$li(strong("Alejandra G. Cabanillas (Demonstrator):"), " garcaba@unican.es"),
            tags$li(strong("Carlos V.C. Weiss (Planning scenarios):"), " carlosvinicius.dacruz@unican.es"),
            tags$li(strong("Renan M. Goncalves (Indicators & data):"), " renan.goncalves@unican.es"),
            tags$li(strong("Camino F. de la Hoz & Elvira Ramos (Species Distribution Models):"), " camino.fernandez@unican.es; elvira.ramos@unican.es"),
            tags$li(strong("Carolina Marti Llambrich (Co-Lead):"), " carolina.marti@udg.edu")
          ),
          br(), br()
        )
      ),

      # =====================================================================
      # TAB 2 - MAP EXPLORER
      # =====================================================================
      tabPanel(
        "Map Explorer",

        div(
          class = "map-wrap",

          leafletOutput("map", height = "calc(100vh - 110px)"),

          # ---- Title, floating top centre ------------------------------
          div(class = "float-panel float-title",
              textOutput("map_title", inline = TRUE)),

          # ---- Left floating panel: controls ---------------------------
          div(
            class = "float-panel float-left",

            selectInput("driver", label_with_info("Driver:", glossary$driver),
                        choices = c(
                          "Fisheries and Climate Change" = "fisheries",
                          "OWE Development"              = "owe",
                          "Data Gaps"                    = "gaps"
                        )),

            # Fisheries-only controls
            conditionalPanel(
              condition = "input.driver == 'fisheries'",
              radioButtons("component",
                           label_with_info("Component:", glossary$component),
                           choiceNames  = component_choice_names(fish_components, glossary_fish),
                           choiceValues = unname(fish_components)),
              hr(),
              checkboxInput("show_sdm",
                            label_with_info("Environmental Suitability", glossary$sdm),
                            value = FALSE),
              conditionalPanel(
                condition = "input.show_sdm == true && input.driver == 'fisheries'",
                selectInput("sdm_species", "Species:", choices = sdm_species_choices)
              )
            ),

            # OWE-only controls
            conditionalPanel(
              condition = "input.driver == 'owe'",
              selectInput("owe_scenario",
                          label_with_info("Scenario:", glossary$scenario),
                          choices = owe_scenarios, selected = "BAU"),
              sliderTextInput("owe_time",
                              label_with_info("Time step:", glossary$time),
                              choices = owe_times, selected = "Production",
                              grid = TRUE, force_edges = TRUE),
              hr(),
              radioButtons("owe_variable",
                           label_with_info("Component:", glossary$component),
                           choiceNames  = component_choice_names(owe_variables, glossary_owe),
                           choiceValues = unname(owe_variables),
                           selected     = "msev")
            ),

            # Data gaps controls
            conditionalPanel(
              condition = "input.driver == 'gaps'",
              selectInput("gap_dataset",
                          label_with_info("Dataset:", glossary$gaps),
                          choices = gap_choices)
            )
          ),

          # ---- Right floating panel: analysis --------------------------
          # Not shown for Data Gaps, which is read from the map alone.
          conditionalPanel(
            condition = "input.driver != 'gaps'",
            div(
              class = "float-panel float-right",

              downloadButton("download_plots", label = NULL,
                             icon  = icon("download"),
                             class = "btn-xs dl-corner",
                             title = "Download all analysis plots for this driver and view (PDF)"),

              conditionalPanel(
                condition = "input.driver == 'fisheries'",
                selectInput("fish_side_plot",
                            label_with_info("Analysis:", fish_analysis_info),
                            choices = fish_side_plots, selected = "msev_top"),
                p(style = "font-size:11px; color:#666;",
                  "Showing visible regions only (based on map extent)."),
                conditionalPanel(
                  condition = "input.fish_side_plot == 'msev_top'",
                  plotOutput("fish_msev_top_plot", height = side_plot_height)
                ),
                conditionalPanel(
                  condition = "input.fish_side_plot == 'eco_comm'",
                  plotOutput("fish_eco_comm_plot", height = side_plot_height)
                ),
                conditionalPanel(
                  condition = "input.fish_side_plot == 'sen_ac'",
                  plotOutput("fish_sen_ac_plot", height = side_plot_height)
                )
              ),

              conditionalPanel(
                condition = "input.driver == 'owe'",
                selectInput("owe_side_plot",
                            label_with_info("Analysis:", owe_analysis_info),
                            choices = owe_side_plots, selected = "range"),
                p(style = "font-size:11px; color:#666;",
                  "Showing visible regions only (based on map extent)."),
                conditionalPanel(
                  condition = "input.owe_side_plot == 'range'",
                  plotOutput("owe_range_plot", height = side_plot_height)
                ),
                conditionalPanel(
                  condition = "input.owe_side_plot == 'country'",
                  plotOutput("owe_country_plot", height = side_plot_height)
                ),
                conditionalPanel(
                  condition = "input.owe_side_plot == 'eco_comm'",
                  plotOutput("owe_eco_comm_plot", height = side_plot_height)
                ),
                conditionalPanel(
                  condition = "input.owe_side_plot == 'sen_ac'",
                  plotOutput("owe_sen_ac_plot", height = side_plot_height)
                )
              )
            )
          )
        ),

        # ---- Below the map: full-width plots ---------------------------
        conditionalPanel(
          condition = "input.driver == 'owe'",
          hr(),
          h5(label_with_info("MSEV across scenarios", owe_movers_info),
             style = "margin-bottom:2px;"),
          plotOutput("owe_movers_plot", height = "560px")
        ),

        conditionalPanel(
          condition = "input.driver == 'fisheries'",
          hr(),
          h5(label_with_info("Adaptative Capacity Components", ac_components_info),
             style = "margin-bottom:2px;"),
          plotOutput("bar_plot", height = "400px")
        )
      ),

      # =====================================================================
      # TAB 3 - SCENARIO DEVELOPMENT
      # =====================================================================
      tabPanel(
        "Scenario Development",
        fluidPage(
          h3("SEADITO Scenario Card Development",
             style = "margin-top:12px;color:#2c3e50;"),
          uiOutput("scenario_card_table"),
          br(), br()
        )
      ),

      # =====================================================================
      # TAB 4 - RAW DATA
      # =====================================================================
      tabPanel(
        "Raw Data",
        fluidPage(
          h2("Raw Data Explorer"),
          p(style = "font-size:12px; color:#666;",
            "Values are shown as stored. Adaptative Capacity here is in
             vulnerability orientation (100 = lowest capacity), the reverse of
             how it is displayed in the Map Explorer."),
          fluidRow(
            column(4, selectInput("raw_driver", "Driver:",
                                  choices = c(
                                    "Fisheries and Climate Change" = "fisheries",
                                    "OWE Development"              = "owe"
                                  ))),

            column(4,
              conditionalPanel(
                condition = "input.raw_driver == 'fisheries'",
                selectInput("raw_component", "Component:",
                            choices = c(
                              "MSEV Results"          = "msev_results",
                              "Community Risk"        = "community_risk",
                              "Community Sensitivity" = "community_sensitivity",
                              "Adaptative Capacity"   = "adaptative_capacity"
                            ))
              ),
              conditionalPanel(
                condition = "input.raw_driver == 'owe'",
                selectInput("raw_owe_component", "Component:",
                            choices = c(
                              "MSEV by scenario and time step" = "owe_msev",
                              "MSEV range per region"          = "owe_msev_range",
                              "Projects and area by subregion" = "owe_subregion"
                            ))
              )
            ),

            column(4,
              conditionalPanel(
                condition = "input.raw_driver == 'fisheries'",
                selectInput("raw_scenario", "Scenario:",
                            choices = c("All", "2050", "2100"), selected = "All")
              ),
              conditionalPanel(
                condition = "input.raw_driver == 'owe'",
                selectInput("raw_owe_scenario", "Scenario:",
                            choices = c("All", owe_scenarios), selected = "All")
              )
            )
          ),
          hr(),
          downloadButton("download_raw", "Download filtered data as CSV"),
          hr(),
          DT::dataTableOutput("raw_table")
        )
      ),

      # =====================================================================
      # TAB 5 - DATA SOURCES
      # =====================================================================
      tabPanel(
        "Data Sources",
        fluidPage(
          h2("Data Sources"),
          p(style = "color:#555;",
            "The raw datasets behind the demonstrator and where they come
             from. Choose a provider and where the data are used, then select
             a row to see its spatial coverage on the map."),
          fluidRow(
            column(5,
                   p(style = "font-size:12px; color:#666;",
                     textOutput("sources_note", inline = TRUE)),
                   leafletOutput("sources_map", height = "600px")),
            column(7,
                   fluidRow(
                     column(6, selectInput("src_provider", "Provider:",
                                           choices = source_provider_choices,
                                           selected = "All")),
                     column(6, selectInput("src_used_in", "Used in:",
                                           choices = source_used_in_choices,
                                           selected = "All"))
                   ),
                   DT::dataTableOutput("sources_table"))
          ),
          br(), br()
        )
      )
    )
  ),

  # Funding logo, bottom right after all page content
  div(class = "footer-logo",
      tags$img(src = "funding.png")),

  # Fixed corner logo (bottom left)
  div(id = "logo-bottom-left",
      div(class = "corner-logo", tags$img(src = "FIHAC.png")))
)


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  #____________________________________________________________________________
  ## Local references ####
  #____________________________________________________________________________

  coastal_local <- coastal_nuts3_area


  #____________________________________________________________________________
  ## Scenario card table ####
  #____________________________________________________________________________

  output$scenario_card_table <- renderUI({
    section_blocks <- lapply(names(scenario_card), function(nm) {
      render_card_section(nm, scenario_card[[nm]])
    })

    tags$table(
      style = "width:100%;border-collapse:collapse;font-size:13px;
               border:1px solid #d5dce3;line-height:1.45;",
      do.call(tagList, section_blocks)
    )
  })


  #____________________________________________________________________________
  ## Map bounds ####
  #____________________________________________________________________________

  map_bounds <- reactiveValues(xmin = NULL, xmax = NULL,
                               ymin = NULL, ymax = NULL)

  observe({
    b <- input$map_bounds
    if (!is.null(b)) {
      map_bounds$xmin <- b$west;  map_bounds$xmax <- b$east
      map_bounds$ymin <- b$south; map_bounds$ymax <- b$north
    }
  })


  #____________________________________________________________________________
  ## Regions currently in view ####
  #____________________________________________________________________________
  # Driver independent, so every driver's side plots subset on the same
  # centroid-in-bounds test.

  codes_in_view <- reactive({
    req(map_bounds$xmin)

    bbox_sf <- st_as_sfc(st_bbox(
      c(xmin = map_bounds$xmin, xmax = map_bounds$xmax,
        ymin = map_bounds$ymin, ymax = map_bounds$ymax),
      crs = st_crs(4326)
    ))

    sf::sf_use_s2(FALSE)
    in_view <- st_within(coastal_centroids, bbox_sf, sparse = FALSE)[, 1]
    sf::sf_use_s2(TRUE)

    coastal_nuts3_area$nuts_code[in_view]
  })


  #____________________________________________________________________________
  ## Map title ####
  #____________________________________________________________________________

  output$map_title <- renderText({
    if (input$driver == "gaps") {
      req(input$gap_dataset)
      return(paste0("Data gaps: ",
                    names(gap_choices)[gap_choices == input$gap_dataset]))
    }

    if (input$driver == "owe") {
      req(input$owe_variable, input$owe_scenario, input$owe_time)
      var_lab <- names(owe_variables)[owe_variables == input$owe_variable]
      run_lab <- if (input$owe_time == "Production") {
        "Production baseline"
      } else {
        paste(input$owe_scenario, input$owe_time)
      }
      return(paste0("OWE ", run_lab, " \u2014 ", var_lab))
    }

    comp_label <- switch(input$component,
      ecological_risk     = "Ecological Risk",
      community_risk      = "Community Risk",
      adaptative_capacity = "Adaptative Capacity",
      msev                = "MSEV"
    )
    paste0("Scenario 2100 \u2014 ", comp_label)
  })


  #____________________________________________________________________________
  ## Fisheries: map data ####
  #____________________________________________________________________________

  map_data <- reactive({
    req(input$driver == "fisheries")
    comp     <- input$component
    value_df <- msev_2100_global %>% dplyr::select(nuts_code, value = all_of(comp))

    coastal_local %>%
      left_join(value_df, by = "nuts_code") %>%
      mutate(
        value = as.numeric(value),
        label = paste0(name, " (", nuts_code, ")",
                       ifelse(is.na(value), " \u2014 No data",
                              paste0(": ", round(value, 3)))),
        popup = paste0(
          "<b>", name, "</b><br>NUTS: ", nuts_code, "<br>",
          ifelse(is.na(value), "No data available",
                 paste0(switch(comp,
                   ecological_risk     = "Ecological Risk: ",
                   community_risk      = "Community Risk: ",
                   adaptative_capacity = "Adaptative Capacity: ",
                   msev                = "MSEV: "),
                   round(value, 3)))
        )
      ) %>%
      filter(!is.na(value))
  })

  visible_regions <- reactive({
    req(input$driver == "fisheries")

    map_data() %>%
      st_drop_geometry() %>%
      filter(nuts_code %in% codes_in_view(), !is.na(value))
  })


  #____________________________________________________________________________
  ## OWE: map data and selected footprints ####
  #____________________________________________________________________________

  owe_map_data <- reactive({
    req(input$driver == "owe",
        input$owe_scenario, input$owe_time, input$owe_variable)

    var_lab <- names(owe_variables)[owe_variables == input$owe_variable]

    vals <- owe_msev_long %>%
      filter(scenario == input$owe_scenario, time == input$owe_time) %>%
      dplyr::select(nuts_code, value = all_of(input$owe_variable),
                    n_projects, owe_area_km2)

    coastal_local %>%
      left_join(vals, by = "nuts_code") %>%
      mutate(
        value = as.numeric(value),
        label = paste0(name, " (", nuts_code, ")",
                       ifelse(is.na(value), " \u2014 No development",
                              paste0(": ", round(value, 1)))),
        popup = paste0(
          "<b>", name, "</b><br>NUTS: ", nuts_code, "<br>",
          input$owe_scenario, " ", input$owe_time, "<br>",
          ifelse(is.na(value), "No development in this run",
                 paste0(var_lab, ": <b>", round(value, 1), "</b><br>",
                        "Projects: ", n_projects, "<br>",
                        "OWE area: ", round(owe_area_km2, 1), " km\u00b2"))
        )
      ) %>%
      filter(!is.na(value))
  })

  # Only the projects the selected run actually carries
  owe_projects <- reactive({
    req(input$driver == "owe", input$owe_scenario, input$owe_time)
    owe_geom_by_run %>%
      filter(scenario == input$owe_scenario, time == input$owe_time) %>%
      mutate(country_code = substr(nuts_code, 1, 2))
  })


  #____________________________________________________________________________
  ## Data gaps: map data ####
  #____________________________________________________________________________

  gap_map_data <- reactive({
    req(input$driver == "gaps", input$gap_dataset)

    coastal_local %>%
      left_join(gap_datasets[[input$gap_dataset]]$status, by = "nuts_code") %>%
      mutate(
        partial = status == "Partial gaps",
        label   = paste0(name, " (", nuts_code, "): ", status,
                         ifelse(partial, paste0(", missing ", missing_vars), "")),
        popup   = paste0("<b>", name, "</b><br>NUTS: ", nuts_code,
                         "<br>Status: <b>", status, "</b>",
                         ifelse(partial, paste0("<br>Missing: ", missing_vars), ""))
      )
  })


  #____________________________________________________________________________
  ## Initial leaflet render ####
  #____________________________________________________________________________

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(basemap_provider) %>%
      setView(lng = 10, lat = 50, zoom = 4)
  })

  outputOptions(output, "map", suspendWhenHidden = FALSE)

  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "Map Explorer"))
      leafletProxy("map") %>% setView(lng = 10, lat = 50, zoom = 4)
  })


  #____________________________________________________________________________
  ## Map layer helper ####
  #____________________________________________________________________________

  # Clears every driver's layers and draws the region choropleth
  draw_regions <- function(data, fill) {
    leafletProxy("map") %>%
      clearGroup("regions") %>%
      clearGroup("projects") %>%
      clearImages() %>%
      clearControls() %>%
      addPolygons(
        data             = data,
        group            = "regions",
        fillColor        = fill,
        fillOpacity      = 0.75,
        color            = "white", weight = 0.8,
        label            = ~label, popup = ~popup,
        labelOptions     = labelOptions(sticky = FALSE),
        highlightOptions = highlightOptions(
          weight = 2.5, color = "#000000",
          fillOpacity = 0.95, bringToFront = TRUE
        )
      )
  }


  #____________________________________________________________________________
  ## Map update, switches on driver ####
  #____________________________________________________________________________
  # Layers are cleared by group rather than with clearShapes(), and labels are
  # non-sticky, so a tooltip open over a redrawn region does not survive the
  # redraw.

  observe({
    driver <- input$driver

    # ---- Data gaps driver ------------------------------------------------
    if (driver == "gaps") {
      req(input$gap_dataset)

      data    <- gap_map_data()
      ds_lab  <- names(gap_choices)[gap_choices == input$gap_dataset]
      present <- factor(intersect(gap_levels, unique(data$status)),
                        levels = gap_levels)

      draw_regions(data, gap_pal(data$status)) %>%
        addLegend(position = "bottomright", pal = gap_pal, values = present,
                  title = paste0("Data gaps<br>", ds_lab), opacity = 0.85,
                  layerId = "main_legend")

      return()
    }

    # ---- OWE driver ------------------------------------------------------
    if (driver == "owe") {
      req(input$owe_variable)

      data    <- owe_map_data()
      var_lab <- names(owe_variables)[owe_variables == input$owe_variable]
      valid   <- data$value[!is.na(data$value)]
      req(length(valid) > 0)

      pal       <- make_pal(input$owe_variable, valid)
      leg_title <- if (input$owe_variable == "adaptative_capacity")
                     paste0(var_lab, "<br>", ac_legend_note) else var_lab

      proxy <- draw_regions(data, pal(data$value))

      projects <- owe_projects()
      if (nrow(projects) > 0)
        proxy <- proxy %>%
          addPolygons(
            data         = projects,
            group        = "projects",
            fillColor    = ~status_pal(STATUS), fillOpacity = 0.9,
            color        = "black", weight = 0.6,
            label        = ~paste0(name, " (", nuts_code, "): ",
                                   round(owe_area_km2, 1), " km\u00b2, ",
                                   STATUS),
            labelOptions = labelOptions(sticky = FALSE),
            highlightOptions = highlightOptions(
              weight = 2.5, color = "#000000", bringToFront = TRUE
            )
          )

      proxy <- proxy %>%
        addLegend(position = "bottomright", pal = pal, values = valid, bins = 4,
                  title = leg_title, opacity = 0.85, layerId = "main_legend")

      if (nrow(projects) > 0)
        proxy %>%
          addLegend(position = "bottomright", pal = status_pal,
                    values = sort(unique(projects$STATUS)),
                    title = paste0("OWE project status (", nrow(projects), ")"),
                    opacity = 0.9, layerId = "owe_legend")

      return()
    }

    # ---- Fisheries driver ------------------------------------------------
    data       <- map_data()
    comp       <- input$component
    valid_vals <- data$value[!is.na(data$value)]
    req(length(valid_vals) > 0)

    pal          <- make_pal(comp, valid_vals)
    legend_title <- names(fish_components)[fish_components == comp]
    if (comp == "adaptative_capacity")
      legend_title <- paste0(legend_title, "<br>", ac_legend_note)

    draw_regions(data, pal(data$value)) %>%
      addLegend(position = "bottomright", pal = pal, values = valid_vals, bins = 4,
                title = legend_title, opacity = 0.85, layerId = "main_legend")
  })


  #____________________________________________________________________________
  ## SDM raster overlay ####
  #____________________________________________________________________________
  # Runs after the map update (lower priority) and also listens to the
  # component, so the overlay is redrawn whenever the map update clears it.
  # Legend sits bottom right with the component legend.

  observe({
    req(input$driver == "fisheries")
    input$component

    proxy <- leafletProxy("map")

    if (!isTRUE(input$show_sdm)) {
      proxy %>% clearImages() %>% removeControl("sdm_legend")
      return()
    }

    species_key <- input$sdm_species
    req(!is.null(species_key), nchar(species_key) > 0)

    r <- load_sdm(species_key)
    if (is.null(r)) return()

    vals      <- raster::values(r)
    vals_trim <- vals[!is.na(vals)]
    pal_sdm   <- colorNumeric(sdm_cols, domain = c(0, 1),
                              na.color = "transparent")

    proxy %>%
      clearImages() %>%
      addRasterImage(r, colors = pal_sdm, opacity = 0.55,
                     layerId = "sdm_raster") %>%
      addLegend(
        position = "bottomright",
        pal      = pal_sdm,
        values   = vals_trim,
        bins     = 4,
        title    = paste0("Suitability<br>",
                          gsub("_", " ", species_key), " (2100)"),
        opacity  = 0.85,
        layerId  = "sdm_legend"
      )
  }, priority = -1)


  #____________________________________________________________________________
  ## Shared scatter for both drivers ####
  #____________________________________________________________________________
  # One point per region, coloured by MSFD subregion. When more than
  # scatter_top_n regions are in view, only those with the highest MSEV are
  # drawn.

  region_scatter <- function(d, x_var, y_var, x_lab, y_lab, colour_var,
                             run_lab, x_axis_lab = x_lab) {
    req(all(c(x_var, y_var, colour_var) %in% names(d)))

    d <- d %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
    req(nrow(d) > 0)

    n_lab <- paste0("n = ", nrow(d))
    if ("msev" %in% names(d) && nrow(d) > scatter_top_n) {
      d     <- d %>% slice_max(as.numeric(msev), n = scatter_top_n, with_ties = FALSE)
      n_lab <- paste0("top ", scatter_top_n, " regions by MSEV")
    }

    n_sub <- length(unique(d[[colour_var]]))
    cols  <- tryCatch(pals::watlington(n = max(n_sub, 1)),
                      error = function(e) scales::hue_pal()(max(n_sub, 1)))

    ggplot(d, aes(x = .data[[x_var]], y = .data[[y_var]], label = name)) +
      geom_point(aes(colour = .data[[colour_var]]), size = 1.8, alpha = 0.85) +
      geom_text_repel(size = 2.4, box.padding = 0.3, point.padding = 0.15,
                      segment.color = "grey70", segment.size = 0.2,
                      max.overlaps = 15, min.segment.length = 0.2) +
      scale_colour_manual(values = cols, name = NULL,
                          labels = function(x) stringr::str_wrap(x, 22)) +
      labs(title    = paste0(y_lab, " vs ", x_lab),
           subtitle = paste0(run_lab, "  |  ", n_lab),
           x = x_axis_lab, y = y_lab) +
      theme_light(base_size = 9) +
      theme(
        plot.title      = element_text(size = 9, face = "bold"),
        plot.subtitle   = element_text(size = 8),
        axis.text       = element_text(size = 7),
        legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        legend.text     = element_text(size = 6),
        legend.margin   = margin(0, 0, 0, 0)
      ) +
      transparent_theme +
      guides(colour = legend_in_rows(n_sub))
  }


  #____________________________________________________________________________
  ## Fisheries analysis plots ####
  #____________________________________________________________________________
  # Single 2100 scenario, so the MSEV plot is one bar per region.

  fish_in_view <- reactive({
    req(input$driver == "fisheries")
    req(nrow(visible_regions()) > 0)
    msev_2100_global %>% filter(nuts_code %in% visible_regions()$nuts_code)
  })

  p_fish_msev_top <- reactive({
    d <- fish_in_view()
    req("msev" %in% names(d))

    msev_limits <- range(as.numeric(msev_2100_global$msev), na.rm = TRUE)

    d <- d %>%
      mutate(msev = as.numeric(msev)) %>%
      filter(!is.na(msev)) %>%
      slice_max(msev, n = fish_msev_top_n, with_ties = FALSE) %>%
      mutate(region_label = fct_reorder(paste0(name, " (", nuts_code, ")"), msev))
    req(nrow(d) > 0)

    ggplot(d, aes(x = msev, y = region_label, fill = msev)) +
      geom_col(width = 0.6) +
      scale_fill_viridis_c(option = "viridis", limits = msev_limits,
                           direction = if (msev_reverse) -1 else 1,
                           guide = "none") +
      labs(title    = paste0("MSEV, top ", nrow(d), " regions in view"),
           subtitle = "Scenario 2100",
           x = "MSEV", y = NULL) +
      theme_light(base_size = 9) +
      theme(
        plot.title    = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.text     = element_text(size = 7),
        axis.title.x  = element_text(size = 8),
        plot.margin   = margin(5, 8, 5, 5)
      ) +
      transparent_theme
  })

  p_fish_eco_comm <- reactive({
    region_scatter(fish_in_view(), "community_risk", "ecological_risk",
                   "Community Risk", "Ecological Risk",
                   "subregion_label", "Scenario 2100")
  })

  p_fish_sen_ac <- reactive({
    region_scatter(fish_in_view(), "adaptive_strength", "community_sensitivity",
                   "Adaptative Capacity", "Community Sensitivity",
                   "subregion_label", "Scenario 2100",
                   x_axis_lab = paste("Adaptative Capacity", ac_legend_note))
  })

  output$fish_msev_top_plot <- renderPlot(p_fish_msev_top(), res = 96, bg = "transparent")
  output$fish_eco_comm_plot <- renderPlot(p_fish_eco_comm(), res = 96, bg = "transparent")
  output$fish_sen_ac_plot   <- renderPlot(p_fish_sen_ac(),   res = 96, bg = "transparent")


  #____________________________________________________________________________
  ## Fisheries bottom plot: adaptative capacity components ####
  #____________________________________________________________________________

  p_bar <- reactive({
    req(input$driver == "fisheries")
    req(nrow(visible_regions()) > 0)

    cap_cols <- unname(ac_capitals)

    # Five regions with the highest mean capital score (= lowest capacity).
    # Capitals are 0 to 1, flipped for display as 1 - x.
    bar_data <- ac_capital_scores %>%
      filter(nuts_code %in% visible_regions()$nuts_code) %>%
      mutate(avg_score = rowMeans(across(all_of(cap_cols)), na.rm = TRUE)) %>%
      slice_max(avg_score, n = ac_top_n, with_ties = FALSE) %>%
      pivot_longer(all_of(cap_cols), names_to = "capital", values_to = "value") %>%
      mutate(
        value      = 1 - as.numeric(value),
        capital    = factor(names(ac_capitals)[match(capital, cap_cols)],
                            levels = names(ac_capitals)),
        name_short = if_else(nchar(name) > 16,
                             paste0(substr(name, 1, 14), "\u2026"), name),
        name_short = factor(name_short, levels = unique(name_short))
      )

    req(nrow(bar_data) > 0)

    n_regions <- length(unique(bar_data$name_short))

    ggplot(bar_data, aes(y = name_short, x = value, fill = name_short)) +
      geom_col(width = 0.7) +
      facet_wrap(~ capital, nrow = 1) +
      scale_fill_brewer(palette = "Set2") +
      scale_x_continuous(limits = c(0, 1),
                         labels = scales::number_format(accuracy = 0.1)) +
      theme_light(base_size = 10) +
      labs(title    = paste0("Adaptative Capacity by livelihood capital: ", n_regions,
                             " regions in view with the lowest capacity"),
           subtitle = "Capital scores shown so that 1 is the highest capacity",
           x = NULL) +
      theme(
        axis.text.y     = element_text(size = 8),
        axis.text.x     = element_text(size = 8, angle = 45, hjust = 1, vjust = 1),
        axis.title.y    = element_blank(),
        legend.position = "none",
        strip.text      = element_text(size = 8, face = "bold"),
        plot.title      = element_text(size = 12, face = "bold"),
        plot.subtitle   = element_text(size = 9),
        panel.spacing   = unit(0.8, "lines"),
        plot.margin     = margin(5, 10, 10, 6)
      ) +
      transparent_theme
  })

  output$bar_plot <- renderPlot(p_bar(), res = 96, bg = "transparent")


  #____________________________________________________________________________
  ## OWE analysis plot 1: MSEV range per region and scenario ####
  #____________________________________________________________________________

  p_owe_range <- reactive({
    req(input$driver == "owe")
    vis <- codes_in_view()
    req(length(vis) > 0)

    d <- owe_msev_range %>% filter(nuts_code %in% vis)
    req(nrow(d) > 0)

    keep <- d %>%
      summarise(ord = max(msev_range, na.rm = TRUE), .by = c(nuts_code, name)) %>%
      slice_max(ord, n = owe_range_top_n, with_ties = FALSE)

    lev <- keep %>%
      arrange(ord) %>%
      mutate(lab = paste0(name, " (", nuts_code, ")")) %>%
      pull(lab)

    # Scenarios in BAU, SUS, Growth order in the legend and, top to bottom,
    # within each region's group of bars
    d %>%
      filter(nuts_code %in% keep$nuts_code) %>%
      mutate(region_label = factor(paste0(name, " (", nuts_code, ")"),
                                   levels = lev),
             scenario     = factor(scenario, levels = owe_scenarios)) %>%
      ggplot(aes(x = msev_range, y = region_label, fill = scenario,
                 group = fct_rev(scenario))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      scale_fill_manual(values = scenario_cols, name = "Scenario") +
      labs(title = paste0("MSEV range, top ", nrow(keep), " regions in view"),
           x = "MSEV range (max - min)", y = NULL) +
      theme_light(base_size = 9) +
      theme(
        plot.title      = element_text(size = 10, face = "bold"),
        axis.text       = element_text(size = 7),
        axis.title.x    = element_text(size = 8),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"),
        legend.text     = element_text(size = 7),
        legend.title    = element_text(size = 7),
        plot.margin     = margin(5, 8, 5, 5)
      ) +
      transparent_theme
  })

  output$owe_range_plot <- renderPlot(p_owe_range(), res = 96, bg = "transparent")


  #____________________________________________________________________________
  ## OWE analysis plot 2: projects and area by country ####
  #____________________________________________________________________________
  # Two panels side by side, so each metric carries its own fill scale: OrRd
  # for project counts, PRGn for area. Totals cover the regions in view only,
  # and only the top countries by number of projects in the selected scenario
  # are drawn.

  p_owe_country <- reactive({
    req(input$driver == "owe", input$owe_scenario)
    vis <- codes_in_view()
    req(length(vis) > 0)

    d_all <- owe_msev_long %>%
      filter(nuts_code %in% vis) %>%
      mutate(country_code = substr(nuts_code, 1, 2)) %>%
      summarise(
        `Number of projects` = sum(n_projects, na.rm = TRUE),
        `OWE area (km2)`     = sum(owe_area_km2, na.rm = TRUE),
        .by = c(scenario, time, country_code)
      ) %>%
      pivot_longer(c(`Number of projects`, `OWE area (km2)`),
                   names_to = "metric", values_to = "value") %>%
      mutate(rel  = value / max(value, na.rm = TRUE), .by = metric) %>%
      mutate(time = factor(time, levels = owe_times))

    d <- d_all %>% filter(scenario == input$owe_scenario, value > 0)
    req(nrow(d) > 0)

    top_countries <- d %>%
      filter(metric == "Number of projects") %>%
      summarise(total = sum(value), .by = country_code) %>%
      slice_max(total, n = owe_country_top_n, with_ties = FALSE) %>%
      pull(country_code)

    d <- d %>% filter(country_code %in% top_countries)

    tile_panel <- function(metric_name, brewer_palette, pal_direction) {
      d %>%
        filter(metric == metric_name) %>%
        ggplot(aes(x = time, y = fct_rev(country_code), fill = rel)) +
        geom_tile(colour = "white", linewidth = 0.4) +
        geom_text(aes(label = round(value)), size = 2.4, colour = "grey15") +
        scale_fill_distiller(palette = brewer_palette, direction = pal_direction,
                             name = NULL, labels = scales::percent,
                             limits = c(0, 1)) +
        labs(title = metric_name, x = NULL, y = NULL) +
        theme_light(base_size = 9) +
        theme(
          plot.title        = element_text(size = 9, face = "bold"),
          axis.text         = element_text(size = 7),
          legend.position   = "bottom",
          legend.direction  = "horizontal",
          legend.key.height = unit(0.25, "cm"),
          legend.key.width  = unit(0.9, "cm"),
          legend.text       = element_text(size = 6),
          legend.margin     = margin(0, 0, 0, 0),
          plot.margin       = margin(4, 6, 2, 4)
        ) +
        transparent_theme
    }

    p_projects <- tile_panel("Number of projects", "OrRd",  1)
    p_area     <- tile_panel("OWE area (km2)",     "PRGn", -1)

    (p_projects | p_area) +
      plot_annotation(
        title = paste0("Projects and area by country: ", input$owe_scenario),
        theme = theme(
          plot.title      = element_text(size = 10, face = "bold"),
          plot.background = element_rect(fill = "transparent", colour = NA)
        )
      )
  })

  output$owe_country_plot <- renderPlot(p_owe_country(), res = 96, bg = "transparent")


  #____________________________________________________________________________
  ## OWE analysis plots 3 and 4: scatters for the selected run ####
  #____________________________________________________________________________

  owe_run_in_view <- reactive({
    req(input$driver == "owe", input$owe_scenario, input$owe_time)
    vis <- codes_in_view()
    req(length(vis) > 0)

    owe_msev_long %>%
      filter(scenario == input$owe_scenario, time == input$owe_time,
             nuts_code %in% vis) %>%
      mutate(country_code = substr(nuts_code, 1, 2))
  })

  owe_run_label <- reactive({
    if (input$owe_time == "Production") "Production baseline"
    else paste(input$owe_scenario, input$owe_time)
  })

  p_owe_eco_comm <- reactive({
    region_scatter(owe_run_in_view(), "comm_risk", "eco_risk",
                   "Community Risk", "Ecological Risk",
                   "subregionN", owe_run_label())
  })

  p_owe_sen_ac <- reactive({
    region_scatter(owe_run_in_view(), "adaptative_capacity", "comm_sen",
                   "Adaptative Capacity", "Community Sensitivity",
                   "subregionN", owe_run_label(),
                   x_axis_lab = paste("Adaptative Capacity", ac_legend_note))
  })

  output$owe_eco_comm_plot <- renderPlot(p_owe_eco_comm(), res = 96, bg = "transparent")
  output$owe_sen_ac_plot   <- renderPlot(p_owe_sen_ac(),   res = 96, bg = "transparent")


  #____________________________________________________________________________
  ## OWE plot beneath the map: biggest movers ####
  #____________________________________________________________________________

  p_owe_movers <- reactive({
    req(input$driver == "owe")
    vis <- codes_in_view()
    req(length(vis) > 0)

    # Production is one shared run, so it collapses to a single point rather
    # than being repeated once per scenario.
    d <- owe_msev_long %>%
      filter(nuts_code %in% vis, !is.na(msev)) %>%
      distinct(nuts_code, run_label, .keep_all = TRUE)
    req(nrow(d) > 0)

    # Legend order: Production first, then BAU, SUS, Growth, each by time step
    run_levels <- d %>%
      distinct(run_label, scenario, time) %>%
      arrange(time != "Production",
              match(scenario, owe_scenarios),
              match(time, owe_times)) %>%
      pull(run_label) %>%
      unique()

    keep <- d %>%
      summarise(range_msev = max(msev) - min(msev), .by = c(nuts_code, name)) %>%
      slice_max(range_msev, n = owe_movers_top_n, with_ties = FALSE)

    d %>%
      filter(nuts_code %in% keep$nuts_code) %>%
      mutate(run_label = factor(run_label, levels = run_levels)) %>%
      ggplot(aes(x = fct_reorder(nuts_code, msev), y = msev, colour = run_label)) +
      geom_line(aes(group = nuts_code), colour = "grey70", linewidth = 0.4) +
      geom_jitter(aes(size = n_projects), alpha = 0.85, height = 0.3, width = 0) +
      coord_flip() +
      scale_colour_viridis_d(option = "turbo", name = NULL) +
      scale_size_area(name = "OWE projects", max_size = 6,
                      breaks = function(x) unique(floor(pretty(x, n = 4)))) +
      labs(title = "MSEV across scenarios", x = NULL, y = "MSEV") +
      theme_light(base_size = 10) +
      theme(
        plot.title      = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.box      = "vertical",
        legend.text     = element_text(size = 8),
        axis.text.y     = element_text(size = 8)
      ) +
      transparent_theme
  })

  output$owe_movers_plot <- renderPlot(p_owe_movers(), res = 96, bg = "transparent")


  #____________________________________________________________________________
  ## Download all plots for the current driver ####
  #____________________________________________________________________________
  # One PDF, one plot per page. Plots that cannot be drawn for the current
  # view are skipped rather than failing the download.

  output$download_plots <- downloadHandler(
    filename = function() paste0("seadito_", input$driver, "_plots.pdf"),
    content  = function(file) {
      plot_list <- switch(input$driver,
        fisheries = list(p_fish_msev_top, p_fish_eco_comm, p_fish_sen_ac, p_bar),
        owe       = list(p_owe_range, p_owe_country, p_owe_eco_comm,
                         p_owe_sen_ac, p_owe_movers),
        list()
      )

      plots <- lapply(plot_list, function(r) tryCatch(r(), error = function(e) NULL))
      plots <- Filter(Negate(is.null), plots)

      if (length(plots) == 0)
        plots <- list(ggplot() +
                        annotate("text", x = 0, y = 0,
                                 label = "No plots available for the current view") +
                        theme_void())

      grDevices::cairo_pdf(file, width = 9, height = 6, onefile = TRUE)
      for (p in plots) print(p)
      grDevices::dev.off()
    }
  )


  #____________________________________________________________________________
  ## Data sources tab ####
  #____________________________________________________________________________
  # Two dropdowns filter the table; the selected row is drawn on the map.
  # "Used in" only offers what the chosen provider actually feeds.

  observeEvent(input$src_provider, {
    used <- if (input$src_provider == "All") raw_sources$`Used in`
            else raw_sources$`Used in`[raw_sources$Provider == input$src_provider]
    choices <- c("All", sort(unique(used)))
    current <- isolate(input$src_used_in)
    updateSelectInput(session, "src_used_in", choices = choices,
                      selected = if (isTRUE(current %in% choices)) current else "All")
  })

  sources_filtered <- reactive({
    req(input$src_provider, input$src_used_in)
    d <- raw_sources
    if (input$src_provider != "All") d <- d %>% filter(Provider == input$src_provider)
    if (input$src_used_in  != "All") d <- d %>% filter(`Used in` == input$src_used_in)
    d
  })

  output$sources_table <- DT::renderDataTable({
    DT::datatable(sources_filtered() %>% dplyr::select(-coverage),
                  rownames = FALSE,
                  selection = list(mode = "single", selected = 1),
                  options = list(dom = "t", paging = FALSE, ordering = FALSE,
                                 autoWidth = TRUE, scrollX = TRUE))
  })

  selected_raw_source <- reactive({
    d <- sources_filtered()
    req(nrow(d) > 0)
    sel <- input$sources_table_rows_selected
    if (length(sel) != 1 || sel > nrow(d)) sel <- 1
    d[sel, ]
  })

  output$sources_note <- renderText({
    cov <- selected_raw_source()$coverage
    if (is.na(cov))
      return("This dataset is not cached in the app, so its coverage is not drawn.")
    if (startsWith(cov, "sdm:"))
      return("Environmental suitability, from 0 (light) to 1 (dark).")
    switch(cov,
      owe_geom = "Shaded: OWE project footprints across all runs.",
      nuts3    = "Shaded: coastal NUTS3 regions in the demonstrator. UK and Turkey are outside the map.",
      "Shaded: coastal NUTS3 regions with a value for this variable."
    )
  })

  # The map is built once and every selection redraws its layers through a
  # proxy, which is more reliable than rebuilding the whole widget. It is
  # rendered while hidden, and redrawn each time the tab is opened so the
  # zoom fits the now visible map.
  output$sources_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(basemap_provider) %>%
      setView(lng = 10, lat = 50, zoom = 3)
  })

  outputOptions(output, "sources_map", suspendWhenHidden = FALSE)

  observe({
    req(identical(input$main_tabs, "Data Sources"))
    cov <- selected_raw_source()$coverage

    proxy <- leafletProxy("sources_map") %>%
      clearShapes() %>%
      clearImages() %>%
      clearControls()

    if (is.na(cov)) {
      proxy %>% setView(lng = 10, lat = 50, zoom = 3)
      return()
    }

    if (startsWith(cov, "sdm:")) {
      r <- load_sdm(sub("^sdm:", "", cov))
      if (is.null(r)) {
        proxy %>% setView(lng = 10, lat = 50, zoom = 3)
        return()
      }
      pal_sdm <- colorNumeric(sdm_cols, domain = c(0, 1), na.color = "transparent")
      proxy %>%
        addRasterImage(r, colors = pal_sdm, opacity = 0.7) %>%
        addLegend(position = "bottomright", pal = pal_sdm, values = c(0, 1),
                  bins = 4, title = "Suitability", opacity = 0.85) %>%
        setView(lng = 10, lat = 50, zoom = 3)
      return()
    }

    covered <- switch(cov,
      owe_geom = owe_geom_by_run %>%
                   distinct(name, nuts_code, .keep_all = TRUE) %>%
                   mutate(cov_label = paste0(name, " (", nuts_code, ")")),
      nuts3    = coastal_local %>%
                   mutate(cov_label = paste0(name, " (", nuts_code, ")")),
      {
        col   <- sub("^ac:", "", cov)
        ac_df <- if (inherits(ac_global, "sf")) sf::st_drop_geometry(ac_global) else ac_global
        codes <- if (col %in% names(ac_df)) ac_df$nuts_code[!is.na(ac_df[[col]])] else character(0)
        coastal_local %>%
          filter(nuts_code %in% codes) %>%
          mutate(cov_label = paste0(name, " (", nuts_code, ")"))
      }
    )

    proxy <- proxy %>%
      addPolygons(data = coastal_local, fillColor = "#d0d0d0", fillOpacity = 0.3,
                  color = "white", weight = 0.5,
                  label = ~paste0(name, " (", nuts_code, ")"),
                  labelOptions = labelOptions(sticky = FALSE))

    if (nrow(covered) == 0) {
      proxy %>% setView(lng = 10, lat = 50, zoom = 3)
      return()
    }

    bb <- unname(sf::st_bbox(covered))

    proxy %>%
      addPolygons(data = covered, fillColor = "#34667d", fillOpacity = 0.6,
                  color = "#07284D", weight = 0.6,
                  label = ~cov_label,
                  labelOptions = labelOptions(sticky = FALSE)) %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4]) %>%
      addLegend(position = "bottomright",
                colors = c("#34667d", "#d0d0d0"),
                labels = c("Covered", "Not covered"),
                opacity = 0.8)
  })


  #____________________________________________________________________________
  ## Raw data tab ####
  #____________________________________________________________________________

  raw_data <- reactive({
    req(input$raw_driver)

    if (input$raw_driver == "owe") {
      req(input$raw_owe_component)
      df <- switch(input$raw_owe_component,
        owe_msev       = owe_msev_long_raw,
        owe_msev_range = owe_msev_range,
        owe_subregion  = owe_by_subregion
      )
      if (!is.null(input$raw_owe_scenario) && input$raw_owe_scenario != "All")
        df <- df %>% filter(scenario == input$raw_owe_scenario)
      # Rows in BAU, SUS, Growth order
      if ("scenario" %in% names(df))
        df <- df %>% arrange(match(scenario, owe_scenarios))
      return(df)
    }

    req(input$raw_component)
    sf <- input$raw_scenario
    switch(input$raw_component,
      msev_results = {
        df <- msev_results_rounded
        if (sf != "All") df <- df %>% filter(scenario == sf)
        df
      },
      community_risk = {
        df <- community_risk_rounded
        if (sf != "All") df <- df %>% filter(scenario == sf)
        df
      },
      community_sensitivity = {
        df <- community_sensitivity_rounded
        if (sf != "All") df <- df %>% filter(scenario == sf)
        df
      },
      adaptative_capacity = ac_global
    )
  })

  output$raw_table <- DT::renderDataTable({
    df <- raw_data()
    if (inherits(df, "sf")) df <- st_drop_geometry(df)
    DT::datatable(df, filter = "top", rownames = FALSE,
                  options = list(pageLength = 15, autoWidth = TRUE,
                                 scrollX = TRUE))
  })

  output$download_raw <- downloadHandler(
    filename = function() {
      if (input$raw_driver == "owe") {
        paste0(input$raw_owe_component,
               if (input$raw_owe_scenario != "All")
                 paste0("_", input$raw_owe_scenario) else "",
               ".csv")
      } else {
        paste0(input$raw_component,
               if (input$raw_scenario != "All")
                 paste0("_", input$raw_scenario) else "",
               ".csv")
      }
    },
    content = function(file) {
      df <- raw_data()
      if (inherits(df, "sf")) df <- st_drop_geometry(df)
      write.csv(df, file, row.names = FALSE)
    }
  )
}


# =============================================================================
# Run app ####
# =============================================================================

shinyApp(ui, server)
