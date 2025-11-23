# ======================
#      LIBRARIES
# ======================
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tigris)
library(sf)
library(cartogram)
library(RColorBrewer)
library(scales)
library(ggplot2)
library(grid)   

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

library(plotly)
library(DT)
library(rlang)

# ======================
#      DATA PREP LS
# ======================
ev  <- readr::read_csv("Electric_Vehicle_Population_Data.csv")
pop <- readr::read_csv("ACSDT5Y2023.B01003-Data.csv")
inc <- readr::read_csv("ACSDT5Y2023.B19013-Data.csv")
urb <- readr::read_csv("Ruralurbancontinuumcodes2023.csv")
wa  <- tigris::counties(state = "WA", cb = TRUE, year = 2024)

pop <- pop[-1, 1:(ncol(pop) - 2)] %>%
  transmute(
    geoid      = GEO_ID,
    name       = NAME,
    population = B01003_001E
  ) %>%
  mutate(
    County = stringr::str_remove(name, " County, Washington") |>
      stringr::str_squish()
  )
pop$population <- as.integer(pop$population)

inc <- inc[-1, 1:(ncol(inc) - 2)] %>%
  transmute(
    geoid  = GEO_ID,
    name   = NAME,
    income = B19013_001E
  ) %>%
  mutate(
    County = stringr::str_remove(name, " County, Washington") |>
      stringr::str_squish()
  )
inc$income <- as.integer(inc$income)

wa <- wa %>%
  transmute(
    geoid = GEOID,
    geom  = geometry,
    County = NAME
  )

urb <- urb %>%
  dplyr::filter(State == "WA", Attribute == "RUCC_2023") %>%
  transmute(
    County         = stringr::str_remove(County_Name, " County") |>
      stringr::str_squish(),
    RuralUrbanCode = as.integer(Value)
  )

ev_county <- ev %>%
  filter(State == "WA") %>%
  count(County, name = "ev_count")

wa_ev <- ev_county %>%
  full_join(pop %>% select(County, population, geoid), by = "County") %>%
  mutate(ev_count = replace_na(ev_count, 0L))

wa_ev_geo <- wa %>%
  left_join(wa_ev, by = "County")

wa_ev_geo <- wa_ev_geo %>%
  mutate(
    ev_per_1000 = dplyr::if_else(
      !is.na(population) & population > 0,
      (ev_count / population) * 1000,
      NA_real_
    )
  )

wa_income_geo <- wa_ev_geo %>%
  left_join(inc, by = "County") %>%
  left_join(urb, by = "County") %>%
  mutate(
    RuralUrbanGroup = case_when(
      RuralUrbanCode <= 3 ~ "Metro (1–3)",
      RuralUrbanCode <= 6 ~ "Suburban (4–6)",
      RuralUrbanCode <= 9 ~ "Rural (7–9)",
      TRUE ~ NA_character_
    )
  )

wa_income_geo <- wa_income_geo %>%
  mutate(
    ev_rank = if_else(
      is.na(ev_per_1000),
      NA_integer_,
      min_rank(dplyr::desc(ev_per_1000))
    ),
    income_rank = if_else(
      is.na(income),
      NA_integer_,
      min_rank(dplyr::desc(income))
    ),
    population_rank = if_else(
      is.na(population),
      NA_integer_,
      min_rank(dplyr::desc(population))
    )
  )
n_counties <- nrow(wa_income_geo)

wa_income_geo <- sf::st_transform(wa_income_geo, 32148)

wa_carto <- cartogram::cartogram_cont(wa_income_geo, "population", itermax = 8)

ev_per_dot <- 250

set.seed(77)
make_county_dots <- function(row) {
  n_dots <- round(row$ev_count / ev_per_dot)
  if (is.na(n_dots) || n_dots < 1) return(NULL)
  pts <- sf::st_sample(x = row$geom, size = n_dots, type = "random")
  if (length(pts) == 0) return(NULL)
  sf::st_sf(County = row$County, geometry = pts)
}
dots_list <- lapply(seq_len(nrow(wa_income_geo)), function(i) make_county_dots(wa_income_geo[i, ]))
wa_ev_dots <- do.call(rbind, dots_list)
if (is.null(wa_ev_dots)) {
  wa_ev_dots <- sf::st_sf(
    County   = character(),
    geometry = sf::st_sfc(crs = sf::st_crs(wa_income_geo))
  )
}

set.seed(77)
make_county_dots_cart <- function(row) {
  n_dots <- round(row$ev_count / ev_per_dot)
  if (is.na(n_dots) || n_dots < 1) return(NULL)
  pts <- sf::st_sample(x = row$geometry, size = n_dots, type = "random")
  if (length(pts) == 0) return(NULL)
  sf::st_sf(County = row$County, geometry = pts, crs = sf::st_crs(wa_carto))
}
dots_list_cart   <- lapply(seq_len(nrow(wa_carto)), function(i) make_county_dots_cart(wa_carto[i, ]))
wa_ev_dots_carto <- do.call(rbind, dots_list_cart)
if (is.null(wa_ev_dots_carto)) {
  wa_ev_dots_carto <- sf::st_sf(
    County   = character(),
    geometry = sf::st_sfc(crs = sf::st_crs(wa_carto))
  )
}

pallete    <- colorRampPalette(RColorBrewer::brewer.pal(9, "BuGn")[2:7])(100)
inc_rng    <- range(wa_income_geo$income, na.rm = TRUE)        # global range
inc_breaks <- pretty(inc_rng, n = 4)

ev_rng <- range(wa_income_geo$ev_per_1000, na.rm = TRUE)

rural_urban_palette <- colorRampPalette(
  c("#54278f", "#807dba", "#74c476", "#fdae61", "#e66101")
)(100)

income_min_slider <- floor(inc_rng[1] / 5000) * 5000
income_max_slider <- ceiling(inc_rng[2] / 5000) * 5000
income_min_k <- floor(income_min_slider / 1000)
income_max_k <- ceiling(income_max_slider / 1000)

rucc_rng <- range(wa_income_geo$RuralUrbanCode, na.rm = TRUE)
rucc_min_slider <- max(1, rucc_rng[1])
rucc_max_slider <- min(9, rucc_rng[2])

state_ev_rate_mean   <- mean(wa_income_geo$ev_per_1000, na.rm = TRUE)
state_income_median  <- median(wa_income_geo$income, na.rm = TRUE)
state_total_ev       <- sum(wa_income_geo$ev_count, na.rm = TRUE)

make_ev_map <- function(poly_active, poly_inactive, dots_sf, selected_names = character()) {
  p <- ggplot()
  
  if (nrow(poly_inactive) > 0) {
    p <- p +
      geom_sf(
        data  = poly_inactive,
        fill  = "grey90",
        color = "white",
        size  = 0.25,
        inherit.aes = FALSE
      )
  }
  
  if (nrow(poly_active) > 0) {
    p <- p +
      geom_sf(
        data = poly_active,
        aes(fill = income),
        color = "white",
        size = 0.25
      ) +
      scale_fill_gradientn(
        colours = pallete,
        limits  = inc_rng,                           
        oob     = scales::squish,
        breaks  = inc_breaks,
        labels  = label_dollar(scale = 0.001, suffix = "k"),
        na.value = "grey95",
        name    = "Median Household Income"
      ) +
      scale_shape_manual(
        name   = "EVs Registered",
        values = c(evdot = 16),
        labels = c(evdot = "250")
      )
  } else {
    p <- p +
      scale_fill_gradientn(
        colours = pallete,
        limits  = inc_rng,
        oob     = scales::squish,
        breaks  = inc_breaks,
        labels  = label_dollar(scale = 0.001, suffix = "k"),
        na.value = "grey95",
        name    = "Median Household Income"
      ) +
      scale_shape_manual(
        name   = "EVs Registered",
        values = c(evdot = 16),
        labels = c(evdot = "250")
      )
  }
  
  if (nrow(dots_sf) > 0) {
    p <- p +
      geom_sf(
        data = dots_sf,
        aes(shape = "evdot"),    
        color = "black",
        alpha = 0.35,
        size  = 0.8,
        inherit.aes = FALSE,
        show.legend = TRUE
      )
  }
  
  if (length(selected_names) > 0) {
    highlight_sf <- poly_active %>%
      dplyr::filter(County %in% selected_names)
    
    if (nrow(highlight_sf) > 0) {
      p <- p +
        geom_sf(
          data  = highlight_sf,
          fill  = NA,
          color = "black",
          size  = 0.8
        )
      
      label_pts <- sf::st_centroid(highlight_sf)
      label_pts_coords <- cbind(
        sf::st_drop_geometry(label_pts),
        sf::st_coordinates(label_pts)
      )
      
      if (!"outside_filter" %in% names(label_pts_coords)) {
        label_pts_coords$outside_filter <- FALSE
      }
      
      p <- p +
        geom_label(
          data  = label_pts_coords,
          aes(
            x = X,
            y = Y,
            label = County,
            fontface = ifelse(outside_filter, "italic", "bold")
          ),
          label.padding = unit(0.15, "lines"),
          label.r       = unit(0.1, "lines"),
          label.size    = 0,
          fill          = "white",
          alpha         = 0.85,
          color         = "black",
          size          = 3
        )
    }
  }
  
  p +
    coord_sf(datum = NA, expand = FALSE) +
    labs(
      caption = "Sources: Washington State Department of Licensing; U.S. Census Bureau"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid    = element_blank(),
      axis.text     = element_blank(),
      axis.title    = element_blank(),
      axis.ticks    = element_blank(),
      plot.caption  = element_text(size = 8, color = "grey30"),
      legend.position = "right",
      legend.title    = element_text(size = 10, face = "bold"),
      legend.text     = element_text(size = 9)
    )
}

# ======================
#      DATA PREP ZW
# ======================

ofm_pop_raw <- readr::read_csv("ofm_pop_sade_county_2020_2024.csv")

ofm_pop_clean <- ofm_pop_raw |>
  mutate(
    across(
      -c(`Area Name`, `Age Group`, Year, `Area ID`),
      ~ as.numeric(gsub("[^0-9\\.\\-]", "", as.character(.)))
    )
  ) |>
  filter(
    `Area Name` != "Washington",
    !is.na(`White Total`),
    `Age Group` != "Total"
  )

ofm_pop_div <- ofm_pop_clean |>
  select(-`Age Group`, -Year, -`Area ID`) |>
  group_by(`Area Name`) |>
  summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

ev_zw <- ev |>
  filter(State == "WA") |>
  count(County, name = "EV_count")

df_div <- dplyr::left_join(
  ev_zw,
  ofm_pop_div,
  by = c("County" = "Area Name")
)

county_df <- df_div |>
  rowwise() |>
  mutate(
    total_population = sum(c_across(ends_with("Total")), na.rm = TRUE),
    diversity_index = {
      nums <- c_across(ends_with("Total"))
      if ("Total" %in% names(nums)) {
        nums <- nums[names(nums) != "Total"]
      }
      shares <- nums / Total
      shares <- shares[!is.na(shares) & shares > 0]
      -sum(shares * log(shares))
    }
  ) |>
  ungroup() |>
  filter(EV_count < 50000)

ofm_pop_age <- ofm_pop_clean |>
  filter(!is.na(`Age Group`)) |>
  mutate(
    age_start = as.numeric(stringr::str_extract(`Age Group`, "^\\d+")),
    age_end   = as.numeric(stringr::str_extract(`Age Group`, "\\d+$")),
    age_mid   = (age_start + age_end) / 2
  )

county_avg_age <- ofm_pop_age |>
  group_by(`Area Name`) |>
  summarise(
    avg_age          = sum(age_mid * Total, na.rm = TRUE) / sum(Total, na.rm = TRUE),
    total_population = sum(Total, na.rm = TRUE),
    .groups = "drop"
  )

df2 <- dplyr::left_join(
  ev_zw,
  county_avg_age,
  by = c("County" = "Area Name")
)

# ======================
#      DATA PREP SR
# ======================

ev_sr <- read.csv("Electric_Vehicle_Population_Data.csv", stringsAsFactors = FALSE)
ghg   <- read.csv("GHG_Reporting_Program_Publication_20251102.csv", stringsAsFactors = FALSE)

wa_counties <- tigris::counties(state = "WA", cb = TRUE, class = "sf") %>%
  mutate(NAME = stringr::str_trim(NAME))

ev_clean <- ev_sr %>%
  mutate(
    County     = stringr::str_trim(County),
    Model.Year = as.integer(Model.Year)
  ) %>%
  filter(
    Model.Year %in% 2012:2023,
    !is.na(County),
    County != ""
  )

cols_to_clean <- c(
  "Reported.Emissions..MTCO2e.",
  "Biogenic.Carbon.Dioxide..MTCO2e.",
  "Carbon.Dioxide..MTCO2e.",
  "Methane..MTCO2e.",
  "Nitrous.Oxide..MTCO2e.",
  "HFCs..MTCO2e.",
  "PFCs..MTCO2e.",
  "Sulfur.Hexafluoride..MTCO2e.",
  "Fluorinated...Other..MTCO2e.",
  "Covered.Emissions..MT.CO2e."
)

ghg <- ghg %>%
  mutate(across(all_of(cols_to_clean), ~ as.numeric(gsub(",", "", .)))) %>%
  mutate(
    County = stringr::str_trim(County),
    Year   = as.integer(Year)
  )

ghg_clean <- ghg %>%
  mutate(
    across(
      c(
        Reported.Emissions..MTCO2e.,
        Carbon.Dioxide..MTCO2e.,
        Methane..MTCO2e.,
        Nitrous.Oxide..MTCO2e.
      ),
      ~ tidyr::replace_na(., 0)
    )
  ) %>%
  mutate(
    Sector          = as.factor(Sector),
    Total_Emissions = Reported.Emissions..MTCO2e. +
      Carbon.Dioxide..MTCO2e. +
      Methane..MTCO2e. +
      Nitrous.Oxide..MTCO2e.
  )

ev_by_county <- ev_clean %>%
  group_by(County, Model.Year) %>%
  summarize(total = n(), .groups = "drop")

ghg_power <- ghg_clean %>%
  filter(Sector == "Power Plants", Year %in% 2012:2023) %>%
  group_by(County, Year) %>%
  summarize(
    total_emissions = sum(Total_Emissions, na.rm = TRUE),
    .groups = "drop"
  )

ev_emissions <- ev_by_county %>%
  inner_join(ghg_power, by = c("County" = "County", "Model.Year" = "Year")) %>%
  mutate(
    emissions_million = total_emissions / 1e6,
    ev_per_million    = total / (emissions_million + 1e-6)
  )

global_min_ev  <- min(ev_emissions$total, na.rm = TRUE)
global_max_ev  <- max(ev_emissions$total, na.rm = TRUE)

global_min_emissions_million <- min(ev_emissions$emissions_million, na.rm = TRUE)
global_max_emissions_million <- max(ev_emissions$emissions_million, na.rm = TRUE)

global_min_ratio <- min(ev_emissions$ev_per_million, na.rm = TRUE)
global_max_ratio <- max(ev_emissions$ev_per_million, na.rm = TRUE)

nbins         <- 12
global_breaks <- seq(0, max(ev_by_county$total, na.rm = TRUE), length.out = nbins + 1)

# ======================
#      DATA PREP SM
# ======================
utility_nicknames <- c(
  "PUGET SOUND ENERGY INC||CITY OF TACOMA - (WA)" = "PSE & Tacoma",
  "PUGET SOUND ENERGY INC" = "PSE",
  "CITY OF SEATTLE - (WA)|CITY OF TACOMA - (WA)" = "Seattle & Tacoma",
  "BONNEVILLE POWER ADMINISTRATION||PUD NO 1 OF CLARK COUNTY - (WA)" = "BPA & Clark County",
  "BONNEVILLE POWER ADMINISTRATION||CITY OF TACOMA - (WA)||PENINSULA LIGHT COMPANY" = "BPA & Tacoma & Peninsula",
  "PUGET SOUND ENERGY INC||PUD NO 1 OF WHATCOM COUNTY" = "PSE & Whatcom County",
  "BONNEVILLE POWER ADMINISTRATION||AVISTA CORP||INLAND POWER & LIGHT COMPANY" = "BPA & Avista & Inland",
  "BONNEVILLE POWER ADMINISTRATION||PUD 1 OF SNOHOMISH COUNTY" = "BPA & Snohomish County"
)

load_data <- function() {
  possible_paths <- c(
    "Electric_Vehicle_Population_Data.csv",
    "~/Desktop/STAT 451/Electric_Vehicle_Population_Data.csv",
    file.path(getwd(), "Electric_Vehicle_Population_Data.csv")
  )
  
  data_path <- NULL
  for (path in possible_paths) {
    if (file.exists(path)) {
      data_path <- path
      break
    }
  }
  
  if (is.null(data_path)) {
    stop("Data file not found. Searched in:\n", 
         paste(possible_paths, collapse = "\n"),
         "\n\nPlease ensure 'Electric_Vehicle_Population_Data.xlsx' is accessible.")
  }
  
  message("Loading data from: ", data_path)
  ev_data <- readr::read_csv(data_path)
  
  original_rows <- nrow(ev_data)
  
  ev_data_clean <- ev_data %>%
    drop_na()
  
  cleaned_rows <- nrow(ev_data_clean)
  removed_rows <- original_rows - cleaned_rows
  
  message(sprintf("Data cleaning complete:"))
  message(sprintf("  - Original rows: %s", scales::comma(original_rows)))
  message(sprintf("  - Rows removed: %s", scales::comma(removed_rows)))
  message(sprintf("  - Final rows: %s", scales::comma(cleaned_rows)))
  
  return(ev_data_clean)
}