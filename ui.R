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
#        UI
# ======================
source("setup.R")

ui <- dashboardPage(
  dashboardHeader(title = "Washington EVs"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sbmenu",
      menuItem("Data Exploration", tabName = "explore", icon = icon("magnifying-glass-chart")),
      menuItem("Li-An Song", tabName = "member1", icon = icon("user")),
      menuItem("Shahzaib Mohammad", tabName = "member2", icon = icon("user")),
      menuItem("Zach Wu", tabName = "member3", icon = icon("user")),
      menuItem("Sangey Rinchen", tabName = "member4", icon = icon("user"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .section-header {
          padding: 10px 15px 5px 15px;
          color: #fff;
          border-bottom: 1px solid #444;
          margin-top: 10px;
        }
        .section-header h4 {
          font-weight: bold;
          font-size: 14px;
        }
      "))
    ),
    
    tabItems(
      # ============= Data Exploration Tab (SM) =============
      tabItem(
        tabName = "explore",
        
        h2(tags$strong("Data Exploration and Quality Check")),
        
        fluidRow(
          box(
            title = "Filters for Data Exploration",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 6,
                tags$h4(icon("search"), "Variable Selection"),
                p("Choose which numeric variable you want to explore."),
                uiOutput("boxplot_column_ui")
              )
            )
          )
        ),
        
        fluidRow(
          box(title = "Distribution Plot", width = 12, status = "warning", solidHeader = TRUE,
              p("Visualize the distribution, central tendency (median marked by a diamond), and outliers for the selected numeric column."),
              plotlyOutput("numeric_boxplot", height = "400px")
          )
        ),
        
        fluidRow(
          box(title = "Missing Value Summary", width = 6, status = "danger", solidHeader = TRUE,
              p("This table shows the count and percentage of missing values (NA) for each column, highlighting potential data quality issues."),
              dataTableOutput("missing_summary")
          ),
          box(title = "Descriptive Statistics (Numeric Columns)", width = 6, status = "info", solidHeader = TRUE,
              p("Key summary statistics for all numeric variables, useful for checking distributions and outliers."),
              dataTableOutput("numeric_summary")
          )
        ),
        
        fluidRow(
          box(title = "Raw Data Viewer (First 100 Rows)", width = 12, status = "success", solidHeader = TRUE,
              p("A searchable, paginated view of the raw dataset."),
              dataTableOutput("raw_data_table")
          )
        )
      ),
      
      # ============= Member 1 Tab (LS) =============
      tabItem(
        tabName = "member1",
        
        fluidRow(
          box(
            title = "Where Do Electric Vehicles Take Root? Uncovering the Socioeconomic and Geographic Forces Shaping EV Adoption Across Washington",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            div(
              style = "text-align:right; margin-bottom:6px;",
              actionButton("reset_selection", "Reset Filters & Highlights", class = "btn-sm")
            ),
            
            fluidRow(
              column(
                width = 4,
                sliderInput(
                  inputId = "rucc_range",
                  label = HTML(
                    "Rural–Urban Continuum Code<br/>
                     <span style='font-size:11px; font-weight:400;'>
                     1 (Most Urban) - 9 (Most Rural)
                     </span>"
                  ),
                  min   = rucc_min_slider,
                  max   = rucc_max_slider,
                  value = c(rucc_min_slider, rucc_max_slider),
                  step  = 1,
                  width = "100%"
                )
              ),
              column(
                width = 4,
                sliderInput(
                  inputId = "income_range",
                  label = HTML(
                    "Median Household Income<br/>
                     <span style='font-size:11px; font-weight:400;'>
                     Thousand US Dollars
                     </span>"
                  ),
                  min   = income_min_k,
                  max   = income_max_k,
                  value = c(income_min_k, income_max_k),
                  step  = 5,
                  width = "100%"
                )
              ),
              column(
                width = 4,
                sliderInput(
                  inputId = "pop_range",
                  label = HTML(
                    "Population<br/>
                     <span style='font-size:11px; font-weight:400;'>
                     Percentile of Counties by Population
                     </span>"
                  ),
                  min   = 0,
                  max   = 100,
                  value = c(0, 100),
                  step  = 5,
                  width = "100%"
                )
              )
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("vb_count", width = 3),
          valueBoxOutput("vb_ev_rate", width = 3),
          valueBoxOutput("vb_income", width = 3),
          valueBoxOutput("vb_total_ev", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Cartographic View",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            tabsetPanel(
              id = "map_view",
              type = "tabs",
              tabPanel(
                title = "Actual Geography",
                value = "actual",
                plotOutput(
                  "ev_map_actual",
                  height = 520,
                  hover = hoverOpts("map_hover_actual", delay = 100, delayType = "debounce", nullOutside = TRUE),
                  click = "map_click_actual"
                )
              ),
              tabPanel(
                title = "Population-Weighted Cartogram",
                value = "cartogram",
                plotOutput(
                  "ev_map_carto",
                  height = 520,
                  hover = hoverOpts("map_hover_carto", delay = 100, delayType = "debounce", nullOutside = TRUE),
                  click = "map_click_carto"
                )
              )
            ),
            
            div(
              style = "margin-top:8px;",
              uiOutput("hover_map_box")
            )
          ),
          box(
            title = "Quantitative View",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput(
              "scatter_ev_income",
              height = 520,
              click  = "scatter_click",
              hover  = hoverOpts("scatter_hover", delay = 100, delayType = "debounce", nullOutside = TRUE)
            ),
            div(
              style = "margin-top:8px;",
              uiOutput("hover_scatter_box")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Comparison Across Highlighted Counties",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            radioButtons(
              "baseline_type",
              label   = "Baseline for Comparison",
              choices = c("State Average" = "state",
                          "Highlighted County" = "county"),
              selected = "state",
              inline   = TRUE
            ),
            
            uiOutput("baseline_selector"),
            
            uiOutput("comparison_panel")
          )
        )
      ),
      
      # ============= Member 2 Tab (SM) =============
      tabItem(
        tabName = "member2",
        
        h2(tags$strong("Range Trend Analysis")),
        
        fluidRow(
          box(
            title = "Filters for Range Trend",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = "selected_make",
                  label = "Filter by Vehicle Brand:",
                  choices = NULL,
                  selected = "All",
                  multiple = TRUE
                )
              ),
              column(
                width = 6,
                sliderInput(
                  inputId = "min_range",
                  label = "Minimum Average Range (Miles):",
                  min = 0, max = 300, value = 50, step = 10
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = tags$strong("Question: Average EV Range Trend by Utility Service Area"),
            width = 12, status = "success", solidHeader = TRUE,
            p("This visualization answers the question: For the Top Brands, how has the average Electric Range trended over the years (2015-2019), and how does the trend differ when comparing the various Electric Utility areas? Use the sidebar filters to refine the brands and the minimum range displayed.")
          )
        ),
        fluidRow(
          valueBoxOutput("avg_range_box", width = 6),
          valueBoxOutput("max_range_box", width = 6)
        ),
        fluidRow(
          box(
            title = "Average Range by Vehicle Brand", width = 6,
            plotOutput("range_trend_make_plot", height = "500px")
          ),
          box(
            title = "Average Range by Electric Utility", width = 6,
            plotOutput("range_trend_utility_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Aggregated Data Table", width = 12,
            dataTableOutput("range_data_table")
          )
        ),
        
        tags$hr(style = "border-top: 2px solid #ccc;"),
        
        h2(tags$strong("County EV Composition")),
        
        fluidRow(
          box(
            title = "Filters for County EV Composition",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 6,
                sliderInput(
                  inputId = "top_n_counties",
                  label = "Number of Top Counties to Display:",
                  min = 5, max = 20, value = 10, step = 1
                )
              ),
              column(
                width = 6,
                selectInput(
                  inputId = "selected_type",
                  label = "Filter by Vehicle Type:",
                  choices = c(
                    "All",
                    "Battery Electric Vehicle (BEV)",
                    "Plug-in Hybrid Electric Vehicle (PHEV)"
                  ),
                  selected = "All"
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = tags$strong("Question: EV Population Split (BEV vs. PHEV) by County"),
            width = 12, status = "info", solidHeader = TRUE,
            p("This visualization answers the question: How is the EV population composed of Battery Electric Vehicles (BEV) and Plug-in Hybrid Electric Vehicles (PHEV) across the most popular counties? Use the sidebar slider to select the number of top counties to display.")
          )
        ),
        fluidRow(
          box(
            title = "EV Population Composition: Top Counties", width = 12,
            plotlyOutput("county_comp_plot", height = "600px")
          )
        )
      ),
      
      # ============= Member 3 Tab (ZW) =============
      tabItem(
        tabName = "member3",
        
        fluidRow(
          box(
            title = "Project Overview",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            p(
              "In this project our group hopes to better understand electric vehicle (EV) adoption across Washington. ",
              "We combine EV registration data with demographic data to explore how factors like age and racial diversity ",
              "relate to EVs per 1,000 residents."
            )
          )
        ),
        
        tags$hr(),
        
        fluidRow(
          box(
            title = "How Does the Average Age of a County Impact EVs per 1,000 People?",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("This section explores the relationship between a county’s average age and its level of EV adoption."),
            p("Use the dropdown to highlight a specific county and see how it compares to others.")
          )
        ),
        
        fluidRow(
          box(
            title = "Select County",
            width = 4,
            selectInput(
              "county_select",
              "Choose a County:",
              choices = sort(unique(df2$County))
            )
          ),
          box(
            title = "County Summary",
            width = 8,
            tableOutput("county_summary")
          )
        ),
        
        fluidRow(
          box(
            title = "EVs vs. Average Age",
            width = 12,
            plotOutput("county_plot")
          )
        ),
        
        tags$hr(),
        
        fluidRow(
          box(
            title = "How Does the Shannon Diversity Index Impact EVs per 1,000 People?",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p(
              "The Shannon Diversity Index measures racial diversity by accounting for both the number of racial groups ",
              "present and how evenly people are distributed across those groups."
            ),
            p(
              "It is calculated by summing each group’s proportion times the natural log of that proportion, ",
              "then taking the negative of that sum."
            ),
            p(tags$b("Higher Shannon Diversity Index values correspond to more diverse counties."))
          )
        ),
        
        fluidRow(
          box(
            title = "Filter by Diversity Index",
            width = 12,
            sliderInput(
              "div_range",
              "Select Diversity Range:",
              min   = 0,
              max   = 1.1,
              value = c(0, 1.1),
              step  = 0.01
            )
          )
        ),
        
        fluidRow(
          box(
            title = "EVs vs Diversity Index",
            width = 12,
            plotOutput("diversity_plot")
          )
        )
      ),
      
      # ============= Member 4 Tab (SR) =============
      tabItem(
        tabName = "member4",
        
        fluidRow(
          box(
            title = "Electric Vehicles in Washington State",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            "This project explores whether EV adoption in Washington has reduced net carbon emissions."
          )
        ),
        
        fluidRow(
          tabBox(
            title = "EVs and Emissions",
            width = 12,
            
            tabPanel(
              "Exploratory",
              
              fluidRow(
                box(
                  title = "Project Question",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  p("This dashboard explores the relationship between EV adoption and emissions in Washington."),
                  p("The primary question of this visualization is as follows:"),
                  p("Has the adoption of electric vehicles in the state of Washington helped reduce carbon 
                     emissions, or has the increased demand for electricity actually increased carbon emissions?"),
                  p("To answer this question, we used the following datasets:"),
                  p("1. https://catalog.data.gov/dataset/electric-vehicle-population-data"),
                  p("2. https://data.wa.gov/Natural-Resources-Environment/GHG-Reporting-Program-Map/gtyb-56w7")
                )
              ),
              
              fluidRow(
                box(
                  title = "Emissions Over Time",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("emissionTrend", height = "400px"),
                  helpText("CO₂ emissions from power plants and petroleum systems over time.")
                )
              ),
              
              fluidRow(
                box(
                  title = "Year Selector",
                  width = 4,
                  status = "warning",
                  solidHeader = TRUE,
                  sliderInput(
                    "year_explore",
                    "Select Year:",
                    min   = 2012,
                    max   = 2023,
                    value = max(ev$`Model Year`, na.rm = TRUE),
                    step  = 1,
                    sep   = ""
                  )
                ),
                box(
                  title = "EV Heat Map by County",
                  width = 8,
                  status = "success",
                  solidHeader = TRUE,
                  plotOutput("evHeatmap", height = "500px"),
                  helpText("Distribution of EV registrations across counties.")
                )
              )
            ),
            
            tabPanel(
              "Explanatory",
              
              fluidRow(
                box(
                  title = "EV Adoption vs. Power Plant Emissions",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("adoption", height = "400px"),
                  helpText(
                    "From the graphs, we can understand that there isn't a clear correlation ",
                    "between EV adoption and emissions on the State level. The next visualization ",
                    "will take a closer look at the county level to answer our question."
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Year Selector",
                  width = 4,
                  status = "warning",
                  solidHeader = TRUE,
                  sliderInput(
                    "year_explain",
                    "Select Year:",
                    min   = 2012,
                    max   = 2023,
                    value = max(ev$`Model Year`, na.rm = TRUE),
                    step  = 1,
                    sep   = ""
                  )
                ),
                box(
                  title = "Map Metric Selector",
                  width = 4,
                  status = "warning",
                  solidHeader = TRUE,
                  radioButtons(
                    "map_metric",
                    "Select Metric:",
                    choices = c(
                      "EV Count"        = "ev",
                      "Emissions"       = "emissions",
                      "EVs per Emission" = "ratio"
                    ),
                    selected = "ev"
                  )
                ),
                box(
                  title = "County-Level EVs vs Emissions Map",
                  width = 8,
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("finale", height = "500px"),
                  helpText(
                    "The idea behind this chloropleth is simple: If EVs had a direct impact on the emissions ",
                    "from power plants, we should notice greater emissions in more counties that follow the ",
                    "increase in registered EVs."
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Conclusion",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  p(
                    "From the chloropleth above, we can see that emissions from power plants did not necessarily ",
                    "follow EV adoption. While that can be explained through different reasons, it is important ",
                    "to note that power plant emissions remained contained within the same few counties and their peak ",
                    "emissions did not correspond with a peak in EV adoption."
                  ),
                  p(
                    "The best example of this is in 2019, where the chloropleth shows high emissions in Lewis County, ",
                    "though the line plot does not show any particular change in EV adoption."
                  ),
                  p(
                    "In conclusion, the answer to our question is that the adoption of electric vehicles did not have a ",
                    "significant impact on CO₂ emissions in Washington state. Whether this is by virtue of electric ",
                    "vehicles being superior, or due to improvements in energy efficiency, requires further research."
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)