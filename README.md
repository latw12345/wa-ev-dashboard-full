# Washington EVs Dashboard

A multi-module Shiny dashboard analyzing **electric vehicle (EV) adoption in Washington State**.  
The application integrates EV registration data with demographic, socioeconomic, geographic, and emissions datasets to examine adoption patterns from multiple analytical perspectives.

---

## 1. Project Goals

This dashboard supports five main analytical objectives:

1. **Data Quality & Exploration**  
   Assess missingness, numeric distributions, and overall data integrity for the primary EV registration dataset.

2. **Geography & Socioeconomic Patterns**  
   Explore how EV adoption varies by county, median household income, population size, and rural–urban classification, using maps, population-weighted cartograms, and scatterplots.

3. **Vehicle Technology & Range Trends**  
   Examine EV electric range (2015–2019) across vehicle manufacturers and electric utilities, and characterize BEV vs PHEV composition across high-adoption counties.

4. **Demographics & Equity**  
   Evaluate relationships between EV adoption, county-level average age, and racial diversity using a Shannon Diversity Index.

5. **EV Adoption vs GHG Emissions**  
   Compare EV adoption patterns with CO₂-equivalent emissions from the EPA GHG Reporting Program, at both statewide and county levels.

---

## 2. Application Architecture

**Core Files**

```r
setup.R     # Data preparation, geographic merges, preprocessing, shared objects
ui.R        # Dashboard layout, sidebar structure, and tab-specific UI
server.R    # Server logic, reactive pipelines, and module-specific processing
```

**Primary Technologies**

- **Shiny Ecosystem:** `shiny`, `shinydashboard`
- **Data Wrangling & Visualization:** `dplyr`, `ggplot2`, `readr`, `tidyr` (tidyverse)
- **Geospatial Processing:** `sf`, `tigris`, `cartogram`
- **Color & Scaling Utilities:** `RColorBrewer`, `scales`, `grid`
- **Interactivity & Tables:** `plotly`, `DT`
- **Programming Infrastructure:** `rlang`

---

## 3. Data Sources

The dashboard integrates multiple datasets.  
The **Electric Vehicle Population Data** is the *primary* dataset driving most analyses.  
All other datasets provide demographic, socioeconomic, geographic, or emissions context.

Because of file size constraints, the EV dataset is **not included in the repository** and must be **manually downloaded as a CSV file**.  
The application has been tested with the version dated **October 18, 2025**.  
The dataset is updated periodically; later versions may work but cannot be guaranteed without adjustment.

### 3.1 Electric Vehicle Population Data (WA Department of Licensing)

**File:** `Electric_Vehicle_Population_Data.csv`  
**Role:** *Primary EV registration dataset*

Each row represents a registered EV in Washington. Key variables include:

- Vehicle Make, Model, and Model Year  
- Electric Vehicle Type (e.g., BEV, PHEV)  
- Electric Range (miles)  
- County and City  
- Electric Utility territory  
- VIN-derived attributes

Used to:

- Compute county-level EV counts and totals  
- Derive **EVs per 1,000 residents** (after joining ACS population data)  
- Analyze range trends (2015–2019)  
- Compare manufacturers and electric utilities  
- Classify BEV vs PHEV composition  
- Support geographic mapping and cartogram visualizations

**Dataset Link:** https://catalog.data.gov/dataset/electric-vehicle-population-data

### 3.2 ACS Total Population (U.S. Census Bureau)

**File:** `ACSDT5Y2023.B01003-Data.csv`  
**Role:** *County-level population denominators*

Processed fields:

- GEO_ID (converted to `geoid`)  
- NAME (cleaned to county name)  
- `B01003_001E` (total population, renamed `population`)

Used to:

- Compute **EVs per 1,000 residents** at the county level  
- Define population-based percentiles and rankings  
- Provide weights for population-weighted cartograms

**Dataset Link:** https://data.census.gov/table/ACSDT5Y2023.B01003?q=B01003:+Total+Population&g=040XX00US53$0500000

### 3.3 ACS Median Household Income (U.S. Census Bureau)

**File:** `ACSDT5Y2023.B19013-Data.csv`  
**Role:** *County-level median household income*

Processed fields:

- GEO_ID (converted to `geoid`)  
- NAME (cleaned to county name)  
- `B19013_001E` (median household income, renamed `income`)

Used to:

- Map median household income by county  
- Analyze income vs **EVs per 1,000 residents**  
- Filter counties by income range (via sliders)  
- Construct income ranking metrics

**Dataset Link:** https://data.census.gov/table/ACSDT5Y2023.B19013?q=B19013:+Median+Household+Income+in+the+Past+12+Months+(in+2024+Inflation-Adjusted+Dollars)&g=040XX00US53$0500000

### 3.4 Rural–Urban Continuum Codes (USDA ERS)

**File:** `Ruralurbancontinuumcodes2023.csv`  
**Role:** *County-level rural–urban classification*

Processed to:

- Filter for Washington State (`State == "WA"`)  
- Extract the `RUCC_2023` attribute as `RuralUrbanCode` (integer 1–9)  
- Clean county names to align with other datasets

Code interpretation:

- `1` = most urban (large metro)  
- `9` = most rural (remote, non-metro)  

Used to:

- Annotate counties with `RuralUrbanCode` values  
- Group counties into `RuralUrbanGroup` categories:  
  - `"Metro (1–3)"`, `"Suburban (4–6)"`, `"Rural (7–9)"`  
- Filter maps and scatterplots by rural–urban range  
- Color-code counties by rural–urban status in scatterplots

**Dataset Link:** https://ers.usda.gov/sites/default/files/_laserfiche/DataFiles/53251/Ruralurbancontinuumcodes2023.csv?v=84002

### 3.5 OFM Population & Race Data (WA Office of Financial Management)

**File:** `ofm_pop_sade_county_2020_2024.csv`  
**Role:** *County-level age- and race-structured population*

Processing steps:

- Convert non-numeric entries to numeric counts for race and age fields  
- Filter to county-level records (excluding state-wide totals)  
- Keep rows with valid race totals and non-total age groups  

Used in two derived datasets:

1. **Racial Diversity (Shannon Diversity Index)**  
   - Average race-group counts over 2020–2024  
   - Compute total county population from race totals  
   - Calculate a Shannon diversity index based on race-group shares  
   - Create `county_df` for **EV count vs diversity** analysis  

2. **Average Age**  
   - Compute age-bin midpoints (`age_mid`) from age-range labels  
   - Calculate weighted average age per county using `Total` population counts  
   - Produce `county_avg_age` and join to EV counts (`df2`)

Supports:

- Scatterplots of **average age vs EVs per 1,000 residents**  
- Scatterplots of **Shannon Diversity Index vs EVs per 1,000 residents**  
- County-level summaries of EV counts, average age, and population

**Dataset Link:** https://ofm.wa.gov/wp-content/uploads/sites/default/files/public/dataresearch/pop/asr/sade/ofm_pop_sade_county_2020_2024.xlsx 

### 3.6 EPA GHG Reporting Program (U.S. Environmental Protection Agency)

**File:** `GHG_Reporting_Program_Publication_20251102.csv`  
**Role:** *Facility-level CO₂-equivalent emissions*

Processing steps:

- Convert comma-formatted numeric fields to numeric values  
- Restrict to selected gases and compute `Total_Emissions`  
- Extract `Year`, `Sector`, `County` and clean county names  
- Focus on sectors of interest (e.g., `"Power Plants"`, `"Petroleum Systems"`)

Used to:

- Build statewide **sector-level emissions time series**  
- Aggregate **power plant emissions by county and year** (`ghg_power`)  
- Combine with EV counts (`ev_by_county`) to produce:  
  - `emissions_million` (million metric tons CO₂-equivalent)  
  - `ev_per_million` (EVs per million metric tons CO₂-equivalent)  
- Support county-level mapping of:
  - Emissions  
  - EV counts  
  - EVs per unit of emissions

**Dataset Link:** https://catalog.data.gov/dataset/ghg-reporting-program-publication

---

## 4. Functional Modules

The Shiny dashboard is organized into four contributor tabs plus a shared data exploration tab. Each tab corresponds to a sidebar menu item.

### 4.1 Data Exploration & Quality Check (Shared)

**Tab:** *Data Exploration* (`tabName = "explore"`)  
**Primary code:** `load_data()`, `filtered_data()`, summary and boxplot outputs (SM section in `server.R`)

Capabilities:

- **Central loader function (`load_data()`)**  
  - Searches for `Electric_Vehicle_Population_Data.csv` in common paths  
  - Reads the CSV with `readr::read_csv()`  
  - Performs a simple cleaning step (`drop_na()`) and reports how many rows were retained  

- **Missing-value summary (`missing_summary`)**  
  - Computes counts and percentages of missing values for each column  

- **Numeric summary statistics (`numeric_summary`)**  
  - Restricts to numeric columns  
  - Provides count, minimum, maximum, mean, median, and standard deviation  

- **Distribution visualization (`numeric_boxplot`)**  
  - Select any numeric variable through a dynamic selector  
  - Interactive Plotly boxplot with median marked explicitly  

- **Raw data viewer (`raw_data_table`)**  
  - Displays the first 100 rows of the cleaned EV dataset  
  - Supports sorting and horizontal scrolling

This module provides shared context for data quality and structure that underpins all subsequent analyses.

### 4.2 Geography, Income & Rural–Urban Analysis (Li-An Song)

**Tab:** *Li-An Song* (`tabName = "member1"`)  
**Focus:** County-level patterns in EV adoption, income, population, and rural–urban classification.

Key prepared objects (in `setup.R`):

- `wa_income_geo`: county geometries with:  
  - EV counts (`ev_count`)  
  - Population (`population`)  
  - EVs per 1,000 residents (`ev_per_1000`)  
  - Median household income (`income`)  
  - Rural–Urban Continuum Code (`RuralUrbanCode`)  
  - Rural–Urban group labels (`RuralUrbanGroup`)  
  - Income, EV, and population ranks  

- `wa_carto`: population-weighted cartogram of `wa_income_geo`  

- `wa_ev_dots`, `wa_ev_dots_carto`: simulated EV points (1 dot ≈ 250 EVs) for each county, in both geographic and cartogram space  

Interactive controls (sliders and buttons):

- Rural–Urban range (`rucc_range`)  
- Median household income range (in thousands) (`income_range`)  
- Population percentile range (`pop_range`)  
- Reset filters and highlighted counties (`reset_selection`)  

Core visual and analytic components:

- **Dual EV maps (`ev_map_actual`, `ev_map_carto`)**  
  - Actual county geography vs population-weighted cartogram  
  - Color-graded by median household income  
  - EV dots overlaid (one per ~250 EVs)  
  - County highlighting and labeling based on user selection  

- **Income vs EV adoption scatterplot (`scatter_ev_income`)**  
  - X-axis: median household income  
  - Y-axis: EVs per 1,000 residents (`ev_per_1000`)  
  - Point size: county population  
  - Color: rural–urban continuum value (`RuralUrbanCode`)  
  - Linear regression line to summarize the relationship  

- **Hover-based “county cards”**  
  - Show EV counts, EVs per 1,000 residents, income, population, and rural–urban codes  
  - Distinguish counties inside vs outside current filters  

- **Value boxes (`vb_count`, `vb_ev_rate`, `vb_income`, `vb_total_ev`)**  
  - Number of highlighted counties  
  - Average EVs per 1,000 residents in the highlighted set (vs state average)  
  - Median household income of highlighted counties (vs state median)  
  - Total EVs in highlighted counties (vs state total)  

- **Comparison panel (`comparison_panel`)**  
  - Compare each highlighted county to either:  
    - Washington State as a whole, or  
    - A selected highlight county (baseline)  
  - Show differences for:
    - EVs per 1,000 residents  
    - Total EV counts  
    - Median household income  
    - Population  
    - Rural–Urban Continuum Code  

This module answers questions about where EVs “take root” in Washington and how socioeconomic and geographic factors align with adoption.

### 4.3 Vehicle Range & County EV Composition (Shahzaib Mohammad)

**Tab:** *Shahzaib Mohammad* (`tabName = "member2"`)  
**Focus:** EV range trends by brand and utility, and BEV vs PHEV composition in high-adoption counties.

Shared filtered data (`filtered_data()`):

- Based on `raw_data` from `load_data()`  
- Keeps only rows with valid, positive numeric values for `Electric Range`  

#### 4.3.1 Range Trend Analysis

Controls:

- Multi-select for vehicle `Make` (`selected_make`)  
- Slider for minimum average range (`min_range`)  

Key processing:

- Top utilities by count (`Electric Utility`)  
- Recoding complex utility names into `electric_utility_nickname` using `utility_nicknames`  
- Aggregation by `Model Year`, `Make`, and `electric_utility_nickname` (2015–2019)  
- Filtering based on minimum range and observation count thresholds  

Outputs:

- **Range KPIs**  
  - Overall average range (2015–2019, after filters) (`avg_range_box`)  
  - Maximum observed average range among filtered groups (`max_range_box`)  

- **Average range trend by vehicle brand (`range_trend_make_plot`)**  
  - X-axis: Model Year (2015–2019)  
  - Y-axis: average electric range  
  - Colored lines by manufacturer (`Make`)  

- **Average range trend by electric utility (`range_trend_utility_plot`)**  
  - X-axis: Model Year (2015–2019)  
  - Y-axis: average electric range  
  - Colored and styled lines by `electric_utility_nickname`  

- **Aggregated results table (`range_data_table`)**  
  - Model Year, Make, Utility nickname, average range, observation count  

#### 4.3.2 County EV Composition (BEV vs PHEV)

Controls:

- Number of top counties by EV count (`top_n_counties`)  
- Filter for EV type (`selected_type`: All / BEV / PHEV)  

Key processing:

- Identify top N counties by total EV count  
- Restrict to those counties and EV types:  
  - `"Battery Electric Vehicle (BEV)"`  
  - `"Plug-in Hybrid Electric Vehicle (PHEV)"`  

Output:

- **Grouped bar chart (`county_comp_plot`)**  
  - X-axis: county (ordered by total EV count)  
  - Y-axis: vehicle count  
  - Fill: EV type (BEV vs PHEV)  
  - Interactive tooltips indicate county, EV type, and count  

This module examines how vehicle technology characteristics and utility territories relate to range and how county-level fleets split between BEVs and PHEVs.

### 4.4 Age & Racial Diversity Analysis (Zach Wu)

**Tab:** *Zach Wu* (`tabName = "member3"`)  
**Focus:** Demographic correlates of EV adoption: average age and racial diversity.

Key derived datasets:

- **Average age dataset (`df2`)**  
  - From `county_avg_age` (weighted age) and `ev_zw` (EV counts)  
  - Contains: County, EV counts, average age, total population  

- **Diversity dataset (`county_df`)**  
  - From `ofm_pop_div` and `ev_zw`  
  - Contains: County, EV count, race totals, total population, Shannon Diversity Index  

#### 4.4.1 Average Age and EV Adoption

Controls:

- County selector (`county_select`)  

Outputs:

- **County summary table (`county_summary`)**  
  - County name  
  - Total EVs  
  - Average age  

- **Scatterplot of EVs vs average age (`county_plot`)**  
  - X-axis: average age  
  - Y-axis: EVs per 1,000 residents (computed from EV counts and population)  
  - Points: all counties  
  - Highlighted point: selected county  
  - Regression line summarizing trend across all counties  

#### 4.4.2 Racial Diversity (Shannon Diversity Index) and EV Adoption

Controls:

- Slider for permitted diversity range (`div_range`)  

Output:

- **Scatterplot of EVs vs Shannon Diversity Index (`diversity_plot`)**  
  - X-axis: diversity index (higher = more racially diverse)  
  - Y-axis: EVs per 1,000 residents (based on EV counts and population)  
  - Point size: total population  
  - Regression line summarizing the relationship  

This module investigates how age structure and racial diversity at the county level relate to EV adoption rates.

### 4.5 EV Adoption & GHG Emissions (Sangey Rinchen)

**Tab:** *Sangey Rinchen* (`tabName = "member4"`)  
**Focus:** Relationships between EV adoption and CO₂-equivalent emissions from power plants and related sectors.

Key derived datasets:

- `ev_by_county`: EV counts by county and model year  
- `ghg_clean`: facility-level emissions with computed `Total_Emissions`  
- `ghg_power`: aggregated power-plant emissions by county and year  
- `ev_emissions`: merged EV counts and power-plant emissions, with:
  - `emissions_million` (million metric tons CO₂-equivalent)  
  - `ev_per_million` (EVs per million metric tons CO₂-equivalent)  

#### 4.5.1 Exploratory View

Controls:

- Year selector for EV heatmap (`year_explore`)  

Outputs:

- **Emissions trend line plot (`emissionTrend`)**  
  - Time series of total emissions by sector (Power Plants, Petroleum Systems)  

- **EV heatmap by county (`evHeatmap`)**  
  - Choropleth map of EV counts by county for a selected year  
  - Uses consistent bins (`global_breaks`) for comparability  

#### 4.5.2 Explanatory View

Controls:

- Year selector for detailed map (`year_explain`)  
- Metric selector (`map_metric`):  
  - `"ev"` – EV count  
  - `"emissions"` – power-plant emissions  
  - `"ratio"` – EVs per million metric tons CO₂-equivalent  

Outputs:

- **Dual-axis line chart (`adoption`)**  
  - EV registrations (thousands) vs power-plant emissions (million metric tons CO₂-equivalent) over time  

- **County-level choropleth map (`finale`)**  
  - Shows either EV counts, emissions, or EVs per unit emissions  
  - Uses distinct color palettes and bins for each metric  

- **Narrative conclusion**  
  - Interprets whether changes in EV adoption are visibly aligned with changes in power-plant emissions, at both statewide and county levels  

This module provides both exploratory and explanatory perspectives on the connection between EV growth and power-sector emissions.

---

## 5. Project Structure

```
├── setup.R
├── ui.R
├── server.R
├── Electric_Vehicle_Population_Data.csv
├── ACSDT5Y2023.B01003-Data.csv
├── ACSDT5Y2023.B19013-Data.csv
├── Ruralurbancontinuumcodes2023.csv
├── ofm_pop_sade_county_2020_2024.csv
└── GHG_Reporting_Program_Publication_20251102.csv
```

All CSV filenames must remain **exactly as listed** for the application to load successfully.

---

## 6. Requirements & Setup (Before Running)

### 6.1 R Environment

- R version **4.2 or higher**  
- **RStudio** is strongly recommended for running and developing the Shiny app

### 6.2 Required Packages

Install all dependencies:

```r
install.packages(c(
  "shiny", "shinydashboard", "tidyverse", "sf", "tigris", "cartogram",
  "RColorBrewer", "scales", "grid", "plotly", "DT", "rlang"
))
```

The code also uses base plotting functions for some maps and options(tigris_use_cache = TRUE, tigris_class = "sf") to speed up geographic data loading.

### 6.3 Data File Placement

Place all required CSV files in the **project root directory**, including the manually downloaded EV dataset:

- `Electric_Vehicle_Population_Data.csv`
- `ACSDT5Y2023.B01003-Data.csv`
- `ACSDT5Y2023.B19013-Data.csv`
- `Ruralurbancontinuumcodes2023.csv`
- `ofm_pop_sade_county_2020_2024.csv`
- `GHG_Reporting_Program_Publication_20251102.csv`

Do **not** change these filenames. The application expects these exact names when loading data.

### 6.4 Data Formatting Requirements

To ensure the dashboard runs correctly:

- All input files must be **UTF-8 encoded CSV files**
- Do not use Excel files (`.xlsx`) directly; convert them to `.csv` first
- The EV dataset must include, at minimum:
   - `Make`, `Model`, `Model Year`
   - `Electric Vehicle Type`
   - `Electric Range`
   - `County`
   - `Electric Utility` 
- Columns referenced in the code (e.g., `Electric Vehicle Type`, `Model Year`, `County`) must not be removed or renamed

### 6.5 Running the Dashboard

To run the dashboard smoothly and ensure all file paths resolve correctly, follow these steps:

1. **Open the project using the RStudio project file**
   
   - Double-click **`Fourth Stage.Rproj`**  
   - This sets your working directory to the correct project root automatically, which is required for all file-loading functions in `setup.R` and `server.R`.

3. **Verify that all required CSV files are in the project root**
   
   - They must be in the **same directory** as `ui.R`, `server.R`, and `setup.R`.  
   - Filenames must **match exactly** as listed in Section 6.3.

4. **Confirm that all required packages are installed**
   
   - See Section 6.2 for the install command.

5. **Run the application**
   
   - From the RStudio console (after `.Rproj` has set the working directory), run:

   ```r
   shiny::runApp()
   ```

5. **Wait for initial data loading**
   
   - The first launch may take a few seconds because:
      - `tigris` downloads and caches county geometries
      - The EV dataset is large and requires preprocessing
      - Cartograms and dot-layer generation require initial computation

Once loaded, the dashboard will open in the RStudio Viewer or your default web browser.
