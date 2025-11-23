# Washington EVs Dashboard

A multi-module Shiny dashboard analyzing **electric vehicle (EV) adoption in Washington State**.  
The application integrates EV registration data with demographic, socioeconomic, geographic, and emissions datasets to examine adoption patterns from multiple analytic perspectives.

## 1. Project Goals

This dashboard supports five major analytical objectives:

1. **Data Quality & Exploration**  
   Inspect missingness, distributions, and overall data integrity for the primary EV dataset.

2. **Geography & Socioeconomic Patterns**  
   Explore how EV adoption varies by county, income, population, and rural–urban status, using maps, cartograms, and scatterplots.

3. **Vehicle Technology & Range Trends**  
   Examine EV electric-range patterns (2015–2019) across manufacturers and utility territories and analyze BEV/PHEV composition.

4. **Demographics & Equity**  
   Evaluate relationships between EV adoption, average age, and racial diversity (Shannon Index).

5. **EV Adoption vs GHG Emissions**  
   Compare EV adoption patterns with CO₂ emissions trends from the EPA GHG Reporting Program.

## 2. Application Architecture

**Core Files**

```
setup.R     # Data preparation, geographic merges, preprocessing
ui.R        # Dashboard UI layout and module structure
server.R    # Server logic, reactive pipelines, visualizations
```


**Primary Technologies**

- **Shiny Ecosystem:** `shiny`, `shinydashboard`
- **Data Wrangling & Visualization:** `dplyr`, `ggplot2`, `readr`, `tidyr` (tidyverse)
- **Geospatial Processing:** `sf`, `tigris`, `cartogram`
- **Color & Scaling Utilities:** `RColorBrewer`, `scales`, `grid`
- **Interactivity & Tables:** `plotly`, `DT`
- **Programming Infrastructure:** `rlang`

## 3. Data Sources

The dashboard integrates multiple datasets.  
The **Electric Vehicle Population Data** is the *main* dataset driving all major analyses.  
All other datasets serve as demographic, socioeconomic, or spatial context.

A note on the EV data:  
The dataset is **too large to include in the repository** and must be manually downloaded as CSV.  
Use the version dated **October 18, 2025**, which is guaranteed to work.  
The dataset updates automatically on Data.gov, and while later versions *usually* work, compatibility is not guaranteed.

**Download Link:**  
https://catalog.data.gov/dataset/electric-vehicle-population-data

### 3.1 Electric Vehicle Population Data (WA Department of Licensing)

**File:** `Electric_Vehicle_Population_Data.csv`  
**Role:** *Primary dataset*

Contains one record per registered EV in Washington, including:

- Make, Model, Model Year  
- EV Type (BEV, PHEV)  
- Electric Range  
- County and City  
- Electric Utility Territory  
- VIN-derived attributes

Used for:

- County-level EV counts and rates  
- EVs per 1,000 residents (joined with ACS data)  
- Range analysis (2015–2019)  
- Brand-level and utility-level comparisons  
- BEV vs PHEV composition  
- Map and cartogram visualizations

**Manual download required** (see link above).

### 3.2 ACS Total Population (U.S. Census Bureau)

**File:** `ACSDT5Y2023.B01003-Data.csv`

Provides 2023 5-year ACS county-level population.  
Used for:

- Population denominators  
- EVs per 1,000 residents  
- Population percentile ranking  
- Cartogram scaling

### 3.3 ACS Median Household Income (U.S. Census Bureau)

**File:** `ACSDT5Y2023.B19013-Data.csv`

Provides 2023 5-year ACS county-level medium household income.
Used for:

- County income mapping  
- Income vs EV adoption scatterplots  
- Filtering by income range  
- Income ranking metrics

### 3.4 Rural–Urban Continuum Codes (USDA ERS)

**File:** `Ruralurbancontinuumcodes2023.csv`

county-level RUCC values range from **1 (most urban)** to **9 (most rural)**.  
Used for:

- Annotate counties  
- Filter by rural–urban level  
- Visualize geography-linked adoption trends

### 3.5 OFM Population & Race Data (WA OFM)

**File:** `ofm_pop_sade_county_2020_2024.csv`

Used for:

- **Average Age** (weighted by age-band midpoints)  
- **Racial Diversity (Shannon Index)**  
- Demographic correlations with EV adoption

### 3.6 EPA GHG Reporting Program

**File:** `GHG_Reporting_Program_Publication_20251102.csv`

Facility-level CO₂-equivalent emissions.  
Used for:

- Sector-level emissions time series  
- County emissions mapping  
- EV-to-emissions comparisons  
- Explanatory adoption–emissions alignment analysis

## 4. Functional Modules

The dashboard is structured into five main sidebar modules.

### 4.1 Data Exploration
- Custom `load_data()` function  
- Missing-value summary  
- Numeric distribution viewer (boxplot)  
- Summary statistics  
- First-100-row preview

### 4.2 Geography, Income & Rural–Urban Analysis
- County merges (EV + income + population + RUCC)
- EVs per 1,000 residents
- Dual maps (geographic + population-weighted cartogram)
- Filters: RUCC, income range, population percentile
- Income vs EV adoption scatterplot

### 4.3 Vehicle Range & County EV Composition
Range:
- Filtered and cleaned EV range values  
- Aggregation by model year, brand, and utility  
- Range KPIs and range tables

Composition:
- Top-N counties  
- BEV vs PHEV composition chart (Plotly)

### 4.4 Age & Racial Diversity Analysis
- Average Age (weighted)  
- Shannon Diversity Index  
- Regression-supported scatterplots  
- County summary tables

### 4.5 EV Adoption & GHG Emissions
- Emissions trends by sector  
- County heat map of EV adoption  
- EV × emissions explanatory comparison  

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

All CSV filenames must remain *exact* for the application to load successfully.

## 6. Requirements & Setup (Before Running)

### 6.1 R Environment
- R version **4.2 or higher**
- RStudio strongly recommended

### 6.2 Required Packages
Install all dependencies:

```r
install.packages(c(
  "shiny", "shinydashboard", "tidyverse", "sf", "tigris", "cartogram",
  "RColorBrewer", "scales", "grid", "plotly", "DT", "rlang"
))
```

### 6.3 Data File Placement
Place all CSV files in the project root directory, including the manually downloaded EV dataset.

Exact filenames must be preserved.

### 6.4 Data Formatting Requirements
- All files must be UTF-8 CSVs
- No Excel files (.xlsx)
- EV dataset must include: Make, Model, Model Year, EV Type, Electric Range, County, Electric Utility
- Do not rename or remove columns

### 6.5 Running the Dashboard
Launch the app using:

```r
shiny::runApp()
```
