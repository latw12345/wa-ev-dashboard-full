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
#       SERVER
# ======================
source("setup.R")

server <- function(input, output, session) {
  # ======================
  #      SERVER LS
  # ======================
  selected_names <- reactiveVal(character())
  
  filters_active <- reactive({
    req(input$rucc_range, input$income_range, input$pop_range)
    
    rucc_filtered <- any(input$rucc_range != c(rucc_min_slider, rucc_max_slider))
    inc_filtered  <- any(input$income_range != c(income_min_k, income_max_k))
    pop_filtered  <- any(input$pop_range  != c(0, 100))
    
    rucc_filtered || inc_filtered || pop_filtered
  })
  
  observeEvent(input$reset_selection, {
    selected_names(character())
    
    updateSliderInput(
      session, "rucc_range",
      value = c(rucc_min_slider, rucc_max_slider)
    )
    updateSliderInput(
      session, "income_range",
      value = c(income_min_k, income_max_k)
    )
    updateSliderInput(
      session, "pop_range",
      value = c(0, 100)
    )
  })
  
  base_geo <- reactive({
    df <- wa_income_geo
    
    if (!is.null(input$rucc_range) && length(input$rucc_range) == 2) {
      df <- df %>%
        filter(
          !is.na(RuralUrbanCode),
          RuralUrbanCode >= input$rucc_range[1],
          RuralUrbanCode <= input$rucc_range[2]
        )
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$income_range) && length(input$income_range) == 2) {
      inc_min <- input$income_range[1] * 1000
      inc_max <- input$income_range[2] * 1000
      df <- df %>%
        filter(
          !is.na(income),
          income >= inc_min,
          income <= inc_max
        )
    }
    
    if (!is.null(input$pop_range) && length(input$pop_range) == 2) {
      pmin <- input$pop_range[1] / 100
      pmax <- input$pop_range[2] / 100
      
      pop_min_thr <- quantile(
        wa_income_geo$population,
        probs = pmin,
        na.rm = TRUE,
        names = FALSE
      )
      pop_max_thr <- quantile(
        wa_income_geo$population,
        probs = pmax,
        na.rm = TRUE,
        names = FALSE
      )
      
      df <- df %>%
        filter(
          !is.na(population),
          population >= pop_min_thr,
          population <= pop_max_thr
        )
    }
    
    df
  })
  
  base_carto <- reactive({
    df <- wa_carto
    
    if (!is.null(input$rucc_range) && length(input$rucc_range) == 2) {
      df <- df %>%
        filter(
          !is.na(RuralUrbanCode),
          RuralUrbanCode >= input$rucc_range[1],
          RuralUrbanCode <= input$rucc_range[2]
        )
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$income_range) && length(input$income_range) == 2) {
      inc_min <- input$income_range[1] * 1000
      inc_max <- input$income_range[2] * 1000
      df <- df %>%
        filter(
          !is.na(income),
          income >= inc_min,
          income <= inc_max
        )
    }
    
    if (!is.null(input$pop_range) && length(input$pop_range) == 2) {
      pmin <- input$pop_range[1] / 100
      pmax <- input$pop_range[2] / 100
      
      pop_min_thr <- quantile(
        wa_income_geo$population,
        probs = pmin,
        na.rm = TRUE,
        names = FALSE
      )
      pop_max_thr <- quantile(
        wa_income_geo$population,
        probs = pmax,
        na.rm = TRUE,
        names = FALSE
      )
      
      df <- df %>%
        filter(
          !is.na(population),
          population >= pop_min_thr,
          population <= pop_max_thr
        )
    }
    
    df
  })
  
  active_actual <- reactive({
    base <- base_geo()          
    sel  <- selected_names()
    
    base <- base %>% dplyr::mutate(outside_filter = FALSE)
    
    if (!filters_active() || length(sel) == 0) {
      return(base)
    }
    
    extra <- wa_income_geo %>%
      dplyr::filter(County %in% sel & !(County %in% base$County)) %>%
      dplyr::mutate(outside_filter = TRUE)
    
    dplyr::bind_rows(base, extra) %>%
      dplyr::distinct(County, .keep_all = TRUE)
  })
  
  inactive_actual <- reactive({
    active <- active_actual()
    if (nrow(active) == 0) {
      wa_income_geo
    } else {
      dplyr::anti_join(
        wa_income_geo,
        active %>% sf::st_drop_geometry() %>% dplyr::select(County),
        by = "County"
      )
    }
  })
  
  active_carto <- reactive({
    base <- base_carto()        
    sel  <- selected_names()
    
    base <- base %>% dplyr::mutate(outside_filter = FALSE)
    
    if (!filters_active() || length(sel) == 0) {
      return(base)
    }
    
    extra <- wa_carto %>%
      dplyr::filter(County %in% sel & !(County %in% base$County)) %>%
      dplyr::mutate(outside_filter = TRUE)
    
    dplyr::bind_rows(base, extra) %>%
      dplyr::distinct(County, .keep_all = TRUE)
  })
  
  inactive_carto <- reactive({
    active <- active_carto()
    if (nrow(active) == 0) {
      wa_carto
    } else {
      dplyr::anti_join(
        wa_carto,
        active %>% sf::st_drop_geometry() %>% dplyr::select(County),
        by = "County"
      )
    }
  })
  
  dots_actual <- reactive({
    active <- active_actual()
    if (nrow(active) == 0) {
      wa_ev_dots[0, ]
    } else {
      dplyr::semi_join(
        wa_ev_dots,
        active %>% sf::st_drop_geometry() %>% dplyr::select(County),
        by = "County"
      )
    }
  })
  
  dots_carto <- reactive({
    active <- active_carto()
    if (nrow(active) == 0) {
      wa_ev_dots_carto[0, ]
    } else {
      dplyr::semi_join(
        wa_ev_dots_carto,
        active %>% sf::st_drop_geometry() %>% dplyr::select(County),
        by = "County"
      )
    }
  })
  
  output$vb_count <- renderValueBox({
    df <- active_actual()
    n_highlight <- nrow(df)
    pct <- if (n_counties > 0) round(100 * n_highlight / n_counties, 1) else NA_real_
    
    subtitle <- if (is.na(pct)) {
      HTML("Highlighted Counties")
    } else {
      HTML(paste0("Highlighted Counties<br/>(", pct, "% of State Total)"))
    }
    
    valueBox(
      value = n_highlight,
      subtitle = subtitle,
      icon = icon("map-marker-alt"),
      color = "teal"
    )
  })
  
  output$vb_ev_rate <- renderValueBox({
    df <- active_actual()
    mean_ev <- mean(df$ev_per_1000, na.rm = TRUE)
    if (is.nan(mean_ev)) mean_ev <- NA_real_
    
    val_text <- if (is.na(mean_ev)) "NA" else paste0(round(mean_ev, 1), " / 1,000")
    
    subtitle <- HTML(
      paste0(
        "EVs per 1,000 People<br/>(State Avg: ",
        round(state_ev_rate_mean, 1),
        " / 1,000)"
      )
    )
    
    valueBox(
      value = val_text,
      subtitle = subtitle,
      icon = icon("bolt"),
      color = "light-blue"
    )
  })
  
  output$vb_income <- renderValueBox({
    df <- active_actual()
    med_inc <- median(df$income, na.rm = TRUE)
    if (is.nan(med_inc)) med_inc <- NA_real_
    
    val_text <- if (is.na(med_inc)) "NA" else dollar(med_inc)
    
    subtitle <- HTML(
      paste0(
        "Median Household Income<br/>(State Med: ",
        dollar(state_income_median),
        ")"
      )
    )
    
    valueBox(
      value = val_text,
      subtitle = subtitle,
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$vb_total_ev <- renderValueBox({
    df <- active_actual()
    total_ev <- sum(df$ev_count, na.rm = TRUE)
    pct <- if (state_total_ev > 0) round(100 * total_ev / state_total_ev, 1) else NA_real_
    
    val_text <- comma(total_ev)
    
    subtitle <- if (is.na(pct)) {
      HTML("EVs in Highlighted Counties")
    } else {
      HTML(paste0("EVs in Highlighted Counties<br/>(", pct, "% of State Total)"))
    }
    
    valueBox(
      value = val_text,
      subtitle = subtitle,
      icon = icon("car"),
      color = "yellow"
    )
  })
  
  output$scatter_ev_income <- renderPlot({
    df_active <- active_actual() %>% sf::st_drop_geometry()
    df_all    <- wa_income_geo   %>% sf::st_drop_geometry()
    
    if (nrow(df_active) == 0) {
      return(
        ggplot() +
          theme_void() +
          xlim(0, 1) + ylim(0, 1) +
          annotate(
            "text", x = 0.5, y = 0.5,
            label = "No Counties Match Current Filters",
            size = 4
          )
      )
    }
    
    df_inactive <- df_all %>%
      dplyr::anti_join(df_active %>% dplyr::select(County), by = "County")
    
    sel    <- selected_names()
    df_sel <- df_active %>% dplyr::filter(County %in% sel)
    df_sel_inside  <- df_sel %>% dplyr::filter(!outside_filter)
    df_sel_outside <- df_sel %>% dplyr::filter(outside_filter)
    
    p <- ggplot() +
      geom_point(
        data = df_inactive,
        aes(x = income, y = ev_per_1000, size = population),
        color = "grey80",
        alpha = 0.4,
        shape = 16,
        na.rm = TRUE
      ) +
      geom_point(
        data = df_active,
        aes(x = income, y = ev_per_1000, color = RuralUrbanCode, size = population),
        alpha = 0.8,
        shape = 16,
        na.rm = TRUE
      ) +
      scale_x_continuous(
        name   = "Median Household Income",
        labels = scales::label_dollar(scale = 0.001, suffix = "k"),
        limits = inc_rng             
      ) +
      scale_y_continuous(
        name   = "EVs per 1,000 People",
        labels = scales::label_number(accuracy = 1),
        limits = ev_rng             
      ) +
      scale_size_continuous(
        name   = "County Population",
        range  = c(2, 9),
        breaks = c(50000, 250000, 1000000),
        labels = c("50k", "250k", "1M")
      ) +
      scale_color_gradientn(
        colours = rural_urban_palette,
        limits  = c(1, 9),
        breaks  = c(1, 3, 5, 7, 9),
        labels  = c("1 (Most Urban)", "3", "5", "7", "9 (Most Rural)"),
        name    = "Rural–Urban Continuum Code"
      ) +
      labs(
        caption = "Sources: Washington State Department of Licensing; U.S. Census Bureau; U.S. Department of Agriculture"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor   = element_blank(),
        legend.position    = "right",
        legend.box         = "vertical",
        legend.title       = element_text(size = 11, face = "bold"),
        legend.text        = element_text(size = 9),
        plot.caption       = element_text(size = 8, hjust = 1, color = "grey30")
      )
    
    if (nrow(df_sel) > 0) {
      p <- p +
        geom_point(
          data = df_sel,
          aes(x = income, y = ev_per_1000, size = population),
          inherit.aes = FALSE,
          shape  = 21,
          fill   = NA,
          color  = "black",
          stroke = 0.9,
          show.legend = FALSE
        )
      
      if (nrow(df_sel_inside) > 0) {
        p <- p +
          geom_label(
            data  = df_sel_inside,
            aes(x = income, y = ev_per_1000, label = County),
            inherit.aes   = FALSE,
            size          = 3,
            fontface      = "bold",
            nudge_y       = max(ev_rng) * 0.030,
            label.padding = unit(0.15, "lines"),
            label.r       = unit(0.1, "lines"),
            label.size    = 0,
            fill          = "white",
            alpha         = 0.85,
            color         = "black"
          )
      }
      
      if (nrow(df_sel_outside) > 0) {
        p <- p +
          geom_label(
            data  = df_sel_outside,
            aes(x = income, y = ev_per_1000, label = County),
            inherit.aes   = FALSE,
            size          = 3,
            fontface      = "bold.italic",
            nudge_y       = max(ev_rng) * 0.030,
            label.padding = unit(0.15, "lines"),
            label.r       = unit(0.1, "lines"),
            label.size    = 0,
            fill          = "white",
            alpha         = 0.85,
            color         = "black"
          )
      }
    }
    
    if (nrow(df_active) >= 2) {                                        
      p <- p +
        geom_smooth(
          data      = df_active,                                       
          aes(x = income, y = ev_per_1000),
          method    = "lm",
          se        = TRUE,
          fullrange = TRUE,     
          color     = "grey25",
          linewidth = 0.9,
          linetype  = "dashed",
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
    
    p
  })
  
  county_card <- function(r, outside_filter = FALSE) {
    tags$div(
      style = "display:inline-block; padding:8px 12px; border-radius:6px;
               background-color: rgba(255,255,255,0.97); box-shadow: 0 1px 4px rgba(0,0,0,0.15);
               border: 1px solid #ddd; font-size:12px; margin-right:8px;
               margin-top:4px; margin-bottom:4px;",
      tags$div(style = "font-weight:600; margin-bottom:4px;", r$County),
      tags$div(
        tags$span(style = "font-weight:500;", "Median Household Income: "),
        tags$span(scales::dollar(r$income)),
        if (!is.na(r$income_rank)) {
          tags$span(style = "color:#777;",
                    paste0("  (Rank ", r$income_rank, " of ", n_counties, ")"))
        }
      ),
      tags$div(
        tags$span(style = "font-weight:500;", "Population: "),
        tags$span(scales::comma(r$population)),
        if (!is.na(r$population_rank)) {
          tags$span(style = "color:#777;",
                    paste0("  (Rank ", r$population_rank, " of ", n_counties, ")"))
        }
      ),
      tags$div(
        tags$span(style = "font-weight:500;", "EVs Registered: "),
        tags$span(
          paste0(
            scales::comma(r$ev_count),
            " (",
            ifelse(
              is.na(r$ev_per_1000),
              "NA per 1,000 People",
              paste0(round(r$ev_per_1000, 1), " per 1,000 People")
            ),
            ")"
          )
        ),
        if (!is.na(r$ev_rank)) {
          tags$span(style = "color:#777;",
                    paste0("  (Rank ", r$ev_rank, " of ", n_counties, ")"))
        }
      ),
      tags$div(
        tags$span(style = "font-weight:500;", "Rural–Urban Continuum Code: "),
        tags$span(
          style = "color:#777;",
          ifelse(
            is.na(r$RuralUrbanCode),
            "NA",
            paste0(r$RuralUrbanCode, " (", r$RuralUrbanGroup, ")")
          )
        )
      ),
      if (outside_filter) {
        tags$div(
          style = "color:#999; font-size:11px; margin-top:2px;",
          "(Outside Current Filters)"
        )
      }
    )
  }
  
  comparison_data <- reactive({
    sel <- selected_names()
    if (length(sel) == 0) return(NULL)   
    
    active_names <- base_geo()$County
    
    df_sel <- wa_income_geo %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(County %in% sel) %>%
      dplyr::mutate(
        outside_filter = !(County %in% active_names)
      )
    
    if (nrow(df_sel) == 0) return(NULL)
    
    baseline_type <- input$baseline_type %||% "state"
    
    if (baseline_type == "county") {
      base_name <- input$baseline_county
      if (is.null(base_name) || !(base_name %in% df_sel$County)) {
        baseline_type <- "state"
      }
    }
    
    if (baseline_type == "state") {
      base_label   <- "Washington State"
      base_ev_1000 <- state_ev_rate_mean           
      base_ev_raw  <- state_total_ev               
      base_inc     <- state_income_median          
      base_pop     <- mean(wa_income_geo$population, na.rm = TRUE)
      base_ru      <- mean(wa_income_geo$RuralUrbanCode, na.rm = TRUE)
      base_county  <- NA_character_
    } else {
      base_row <- df_sel %>%
        dplyr::filter(County == input$baseline_county) %>%
        dplyr::slice(1)
      
      base_label   <- base_row$County
      base_ev_1000 <- base_row$ev_per_1000
      base_ev_raw  <- base_row$ev_count
      base_inc     <- base_row$income
      base_pop     <- base_row$population
      base_ru      <- base_row$RuralUrbanCode
      base_county  <- base_row$County
    }
    
    df_out <- df_sel %>%
      dplyr::mutate(
        ev_diff_1000 = ev_per_1000      - base_ev_1000,
        ev_diff_raw  = ev_count         - base_ev_raw,
        inc_diff     = income           - base_inc,
        pop_diff     = population       - base_pop,
        ru_diff      = RuralUrbanCode   - base_ru
      )
    
    list(
      df          = df_out,
      base_label  = base_label,
      base_county = base_county
    )
  })
  
  output$baseline_selector <- renderUI({
    cd <- comparison_data()
    sel <- selected_names()
    
    if (is.null(cd) || input$baseline_type != "county" || length(sel) < 1) {
      return(NULL)
    }
    
    current <- input$baseline_county
    if (is.null(current) || !(current %in% sel)) {
      current <- sel[1]
    }
    
    selectInput(
      "baseline_county",
      label   = "Baseline County",
      choices = sel,
      selected = current,
      width    = "300px"
    )
  })
  
  diff_cell <- function(diff, fmt_fun, higher_good = TRUE, neutral = FALSE) {
    if (is.na(diff)) {
      return(tags$span("NA", style = "color:#999;"))
    }
    
    if (neutral) {
      return(
        tags$span(
          fmt_fun(diff),
          style = "color:#777; margin-left:4px;"
        )
      )
    }
    
    dir <- sign(diff)
    col <- "#999999"
    icon_name <- "minus"
    
    if (dir > 0) {
      col <- if (higher_good) "#1a9850" else "#d73027"  
      icon_name <- "arrow-up"
    } else if (dir < 0) {
      col <- if (higher_good) "#d73027" else "#1a9850"  
      icon_name <- "arrow-down"
    }
    
    tags$span(
      icon(icon_name),
      fmt_fun(abs(diff)),
      style = paste0("color:", col, "; margin-left:4px;")
    )
  }
  
  output$comparison_panel <- renderUI({
    cd <- comparison_data()
    if (is.null(cd)) {
      return(
        tags$small(
          style = "color:#777;",
          "Select at least one county (two or more for relative comparisons). ",
          "Slider filters affect the maps and KPIs, but the table below always ",
          "compares all highlighted counties. Counties outside the current filters ",
          "are flagged explicitly."
        )
      )
    }
    
    df  <- cd$df
    base_label <- cd$base_label
    
    df <- df %>% dplyr::arrange(dplyr::desc(ev_per_1000))
    
    header <- tags$tr(
      tags$th("County"),
      tags$th("EVs per 1,000 People (vs Baseline)"),
      tags$th("EVs Registered (vs Baseline)"),
      tags$th("Median Household Income (vs Baseline)"),
      tags$th("Population (vs Baseline)"),
      tags$th("Rural–Urban Continuum Code (vs Baseline)")
    )
    
    rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      outside_flag <- isTRUE(r$outside_filter)
      
      tags$tr(
        tags$td(
          tags$span(
            if (outside_flag) tags$i(r$County) else r$County,
            style = "font-weight:600;"
          ),
          if (outside_flag) {
            tags$span(
              "  (Outside Current Filters)",
              style = "color:#999; font-size:11px; margin-left:4px;"
            )
          }
        ),
        tags$td(
          tags$span(
            ifelse(
              is.na(r$ev_per_1000),
              "NA",
              sprintf("%.1f", r$ev_per_1000)
            ),
            style = "margin-right:6px;"
          ),
          diff_cell(
            r$ev_diff_1000,
            fmt_fun = function(x) sprintf(" (%.1f)", x),
            higher_good = TRUE
          )
        ),
        tags$td(
          tags$span(
            ifelse(
              is.na(r$ev_count),
              "NA",
              scales::comma(r$ev_count)
            ),
            style = "margin-right:6px;"
          ),
          diff_cell(
            r$ev_diff_raw,
            fmt_fun = function(x) paste0(" (", scales::comma(x), ")"),
            higher_good = TRUE
          )
        ),
        tags$td(
          tags$span(
            ifelse(
              is.na(r$income),
              "NA",
              scales::dollar(r$income)
            ),
            style = "margin-right:6px;"
          ),
          diff_cell(
            r$inc_diff,
            fmt_fun = function(x) paste0(" (", scales::dollar(x), ")"),
            higher_good = TRUE
          )
        ),
        tags$td(
          tags$span(
            ifelse(
              is.na(r$population),
              "NA",
              scales::comma(r$population)
            ),
            style = "margin-right:6px;"
          ),
          diff_cell(
            r$pop_diff,
            fmt_fun = function(x) paste0(" (", scales::comma(x), ")"),
            higher_good = TRUE
          )
        ),
        tags$td(
          tags$span(
            ifelse(
              is.na(r$RuralUrbanCode),
              "NA",
              r$RuralUrbanCode
            ),
            style = "margin-right:6px;"
          ),
          diff_cell(
            r$ru_diff,
            fmt_fun = function(x) sprintf(" (%.1f levels)", x),
            higher_good = FALSE,   
            neutral = FALSE        
          )
        )
      )
    })
    
    tagList(
      tags$div(
        style = "margin-bottom:6px; font-size:12px; color:#555;",
        "Baseline: ",
        tags$span(style = "font-weight:600;", base_label),
        if (input$baseline_type == "state") {
          " (Washington State)"
        } else {
          " (Selected Highlighted County)"
        }
      ),
      tags$table(
        class = "table table-condensed",
        style = "font-size:12px; width:100%;",
        header,
        rows
      )
    )
  })
  
  output$ev_map_actual <- renderPlot({
    sel_names <- selected_names()
    make_ev_map(
      poly_active   = active_actual(),
      poly_inactive = inactive_actual(),
      dots_sf       = dots_actual(),
      selected_names = sel_names
    )
  })
  
  output$ev_map_carto <- renderPlot({
    sel_names <- selected_names()
    make_ev_map(
      poly_active   = active_carto(),
      poly_inactive = inactive_carto(),
      dots_sf       = dots_carto(),
      selected_names = sel_names
    )
  })
  
  observeEvent(input$map_click_actual, {
    click <- input$map_click_actual
    if (is.null(click)) return()
    
    polys <- wa_income_geo
    if (nrow(polys) == 0) return()
    
    pt <- sf::st_sfc(sf::st_point(c(click$x, click$y)), crs = sf::st_crs(polys))
    idx_list <- sf::st_within(pt, polys, sparse = TRUE)[[1]]
    if (length(idx_list) == 0) return()
    
    row  <- polys[idx_list[1], ] |> sf::st_drop_geometry()
    name <- row$County
    
    current <- selected_names()
    if (name %in% current) {
      selected_names(setdiff(current, name))
    } else {
      selected_names(c(current, name))
    }
  })
  
  observeEvent(input$map_click_carto, {
    click <- input$map_click_carto
    if (is.null(click)) return()
    
    polys <- wa_carto
    if (nrow(polys) == 0) return()
    
    pt <- sf::st_sfc(sf::st_point(c(click$x, click$y)), crs = sf::st_crs(polys))
    idx_list <- sf::st_within(pt, polys, sparse = TRUE)[[1]]
    if (length(idx_list) == 0) return()
    
    row  <- polys[idx_list[1], ] |> sf::st_drop_geometry()
    name <- row$County
    
    current <- selected_names()
    if (name %in% current) {
      selected_names(setdiff(current, name))
    } else {
      selected_names(c(current, name))
    }
  })
  
  observeEvent(input$scatter_click, {
    click <- input$scatter_click
    if (is.null(click)) return()
    
    df <- wa_income_geo %>% sf::st_drop_geometry()    
    if (nrow(df) == 0) return()
    
    xrange <- range(df$income, na.rm = TRUE)
    yrange <- range(df$ev_per_1000, na.rm = TRUE)
    if (diff(xrange) == 0 || diff(yrange) == 0) return()
    
    dx <- (df$income      - click$x) / diff(xrange)
    dy <- (df$ev_per_1000 - click$y) / diff(yrange)
    dist2 <- dx^2 + dy^2
    
    i <- which.min(dist2)
    county <- df$County[i]
    
    cur <- selected_names()
    if (county %in% cur) {
      selected_names(setdiff(cur, county))   
    } else {
      selected_names(c(cur, county))        
    }
  })
  
  output$hover_map_box <- renderUI({
    if (identical(input$map_view, "cartogram")) {
      polys <- wa_carto
      h <- input$map_hover_carto
    } else {
      polys <- wa_income_geo
      h <- input$map_hover_actual
    }
    
    if (is.null(h) || nrow(polys) == 0) {
      return(NULL)
    }
    
    pt <- sf::st_sfc(sf::st_point(c(h$x, h$y)), crs = sf::st_crs(polys))
    idx_list <- sf::st_within(pt, polys, sparse = TRUE)[[1]]
    if (length(idx_list) == 0) return(NULL)
    
    row <- polys[idx_list[1], ] |> sf::st_drop_geometry()
    
    active_set <- if (identical(input$map_view, "cartogram")) active_carto() else active_actual()
    active_names <- active_set$County
    outside <- !(row$County %in% active_names)
    
    county_card(row, outside_filter = outside)
  })
  
  output$hover_scatter_box <- renderUI({
    h <- input$scatter_hover
    if (is.null(h)) return(NULL)
    
    df <- wa_income_geo %>% sf::st_drop_geometry()    
    if (nrow(df) == 0) return(NULL)
    
    xrange <- range(df$income, na.rm = TRUE)
    yrange <- range(df$ev_per_1000, na.rm = TRUE)
    if (diff(xrange) == 0 || diff(yrange) == 0) return(NULL)
    
    dx <- (df$income      - h$x) / diff(xrange)
    dy <- (df$ev_per_1000 - h$y) / diff(yrange)
    dist2 <- dx^2 + dy^2
    
    i <- which.min(dist2)
    row <- df[i, ] %>% sf::st_drop_geometry()
    
    county_card(row, outside_filter = FALSE)
  })
  
  # ======================
  #      SERVER ZW
  # ======================

  county_data <- reactive({
    req(input$county_select)
    df2[df2$County == input$county_select, ]
  })
  
  output$county_summary <- renderTable({
    dat <- county_data()
    data.frame(
      County    = input$county_select,
      Total_EVs = sum(dat$EV_count, na.rm = TRUE),
      Avg_Age   = round(mean(dat$avg_age, na.rm = TRUE), 2)
    )
  })
  
  output$county_plot <- renderPlot({
    req(input$county_select)
    
    df_summary <- df2 %>%
      group_by(County) %>%
      summarise(
        total_evs        = sum(EV_count, na.rm = TRUE),
        avg_age          = mean(avg_age, na.rm = TRUE),
        total_population = sum(total_population, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(
      df_summary,
      aes(
        x = avg_age,
        y = (total_evs / total_population * 1000)
      )
    ) +
      geom_point(color = "gray70", size = 3, alpha = 0.7) +
      geom_point(
        data = df_summary[df_summary$County == input$county_select, ],
        aes(x = avg_age, y = (total_evs / total_population * 1000)),
        color = "red", size = 5
      ) +
      geom_smooth(method = "lm", se = FALSE, color = "#f03b20") +
      labs(
        title    = "EVs per 1,000 vs Average Age by County",
        subtitle = paste("Highlighting:", input$county_select),
        x        = "Average Age",
        y        = "EVs per 1,000 People"
      ) +
      theme_minimal(base_size = 14)
  })
  
  filtered_div <- reactive({
    county_df %>%
      filter(
        diversity_index >= input$div_range[1],
        diversity_index <= input$div_range[2]
      )
  })
  
  output$diversity_plot <- renderPlot({
    df_plot <- filtered_div()
    
    ggplot(
      df_plot,
      aes(
        x    = diversity_index,
        y    = EV_count / Total * 1000,
        size = Total
      )
    ) +
      geom_point(color = "black", alpha = 0.6) +
      geom_smooth(
        method = "lm",
        se     = TRUE,
        color  = "#f03b20",
        show.legend = FALSE
      ) +
      scale_size(name = "Population Size") +
      labs(
        title = "EV Registrations vs Population Diversity",
        x     = "Shannon Diversity Index",
        y     = "EVs per 1,000 People"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # ======================
  #      SERVER SR
  # ======================
  
  output$emissionTrend <- renderPlot({
    trend_data <- ghg_clean %>%
      filter(Sector %in% c("Power Plants", "Petroleum Systems")) %>%
      group_by(Year, Sector) %>%
      summarize(Yearly_total = sum(Total_Emissions), .groups = "drop")
    
    ggplot(trend_data, aes(x = Year, y = Yearly_total, colour = Sector)) +
      geom_point() +
      geom_smooth(se = FALSE) +
      labs(
        y = "Total CO2 emissions (Million MTCO2e)",
        title = "CO2 emissions in WA state from sectors of interest"
      ) +
      scale_y_continuous(
        breaks = seq(0, 24000000, 2400000),
        labels = scales::label_number(accuracy = 1, scale = 1e-6, suffix = "M")
      ) +
      theme_minimal()
  })
  
  output$evHeatmap <- renderPlot({
    year_data <- ev_by_county %>% filter(Model.Year == input$year_explore)
    
    map_data <- wa_counties %>%
      left_join(year_data, by = c("NAME" = "County")) %>%
      mutate(total = replace_na(total, 0))
    
    plot(
      map_data["total"],
      breaks = global_breaks,
      pal = function(n) hcl.colors(n, "Inferno", rev = TRUE),
      main = paste("EV Counts by County (", input$year_explore, ")", sep = "")
    )
  })
  
  output$adoption <- renderPlot({
    ev_trend <- ev_by_county %>%
      group_by(Model.Year) %>%
      summarize(total_ev = sum(total), .groups = "drop")
    
    emissions_trend <- ghg_power %>%
      group_by(Year) %>%
      summarize(total_emissions = sum(total_emissions), .groups = "drop")
    
    ggplot() +
      geom_line(
        data = emissions_trend,
        aes(x = Year, y = total_emissions / 1e6),
        color = "red", size = 1
      ) +
      geom_line(
        data = ev_trend,
        aes(x = Model.Year, y = total_ev / 1000),
        color = "blue", size = 1.2
      ) +
      scale_y_continuous(
        name = "Emissions (Million MTCO2e)",
        sec.axis = sec_axis(~ ., name = "EV Registrations (Thousands)")
      ) +
      labs(title = "EV Adoption vs. Power Plant Emissions in WA") +
      theme_minimal()
  })
  
  output$finale <- renderPlot({
    year_data <- ev_emissions %>% filter(Model.Year == input$year_explain)
    
    map_data <- wa_counties %>%
      left_join(year_data, by = c("NAME" = "County")) %>%
      mutate(
        total            = replace_na(total, 0),
        emissions_million = replace_na(emissions_million, 0),
        ev_per_million    = replace_na(ev_per_million, 0)
      )
    
    if (input$map_metric == "emissions") {
      plot(
        map_data["emissions_million"],
        breaks = seq(global_min_emissions_million, global_max_emissions_million, length.out = 12),
        pal = function(n) hcl.colors(n, "Reds", rev = TRUE),
        main = paste("Power Plant Emissions by County (", input$year_explain, ")", sep = ""),
        key.pos = 1,
        key.title = "Emissions (Million Metric Tons CO₂)"
      )
    } else if (input$map_metric == "ev") {
      plot(
        map_data["total"],
        breaks = seq(global_min_ev, global_max_ev, length.out = 12),
        pal = function(n) hcl.colors(n, "Blues", rev = TRUE),
        main = paste("EV Counts by County (", input$year_explain, ")", sep = ""),
        key.pos = 1,
        key.title = "EV Count (Vehicles)"
      )
    } else {
      plot(
        map_data["ev_per_million"],
        breaks = seq(global_min_ratio, global_max_ratio, length.out = 12),
        pal = function(n) hcl.colors(n, "Greens", rev = TRUE),
        main = paste("EVs per Emission by County (", input$year_explain, ")", sep = ""),
        key.pos = 1,
        key.title = "EVs per Million Metric Tons CO₂"
      )
    }
  })
  
  # ======================
  #      SERVER SM
  # ======================
  raw_data <- load_data()
  start_year <- 2015
  end_year <- 2019
  
  filtered_data <- reactive({
    data <- raw_data %>%
      mutate(`Electric Range` = suppressWarnings(as.numeric(`Electric Range`))) %>%
      filter(!is.na(`Electric Range`),
             `Electric Range` > 0)
    
    return(data)
  })
  
  numeric_cols <- reactive({
    filtered_data() %>%
      select(where(is.numeric)) %>%
      colnames()
  })
  
  output$boxplot_column_ui <- renderUI({
    req(numeric_cols())
    selectInput(
      inputId = "boxplot_column",
      label = tags$strong("Variable to Plot:"),
      choices = numeric_cols(),
      selected = if ("Electric Range" %in% numeric_cols()) "Electric Range" else numeric_cols()[1]
    )
  })
  
  observe({
    req(filtered_data())
    valid_makes <- filtered_data() %>%
      count(`Make`) %>%
      filter(n > 10) %>%
      pull(`Make`)
    
    unique_makes <- sort(unique(valid_makes))
    
    updateSelectInput(
      session, 
      "selected_make", 
      choices = c("All", unique_makes),
      selected = "All"
    )
  })
  
  output$missing_summary <- renderDataTable({
    data <- filtered_data()
    
    missing_summary_df <- data %>%
      summarise(across(everything(), ~sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count") %>%
      mutate(
        Total_Rows = nrow(data),
        `Missing %` = round((Missing_Count / Total_Rows) * 100, 2)
      ) %>%
      select(Column, `Missing Count` = Missing_Count, `Missing %`) %>%
      mutate(
        `Missing Count` = scales::comma(`Missing Count`),
        `Missing %` = paste0(`Missing %`, "%")
      )
    
    datatable(
      missing_summary_df,
      options = list(pageLength = 10, dom = 't', ordering = FALSE),
      rownames = FALSE
    )
  })
  
  output$numeric_summary <- renderDataTable({
    numeric_data <- filtered_data() %>%
      select(where(is.numeric))
    
    if (ncol(numeric_data) == 0) {
      return(data.frame(Message = "No numeric columns found.") %>% datatable(options = list(dom = 't'), rownames = FALSE))
    }
    
    summary_stats <- numeric_data %>%
      summarise(across(everything(), list(
        Count = ~ sum(!is.na(.)),
        Min = ~ min(., na.rm = TRUE),
        Max = ~ max(., na.rm = TRUE),
        Mean = ~ mean(., na.rm = TRUE),
        Median = ~ median(., na.rm = TRUE),
        SD = ~ sd(., na.rm = TRUE)
      ), .names = "{.fn}__{.col}")) %>%
      
      pivot_longer(everything(), names_to = "col", values_to = "value") %>%
      separate(col, into = c("Statistic", "Column"), sep = "__") %>%
      pivot_wider(names_from = Statistic, values_from = value) %>%
      
      mutate(across(c(Min, Max, Mean, Median, SD), ~ round(., 2))) %>%
      mutate(Count = scales::comma(Count))
    
    datatable(
      summary_stats,
      options = list(pageLength = 10, dom = 't', ordering = FALSE),
      rownames = FALSE
    )
  })
  
  output$raw_data_table <- renderDataTable({
    filtered_data() %>%
      head(100) %>% 
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  output$numeric_boxplot <- renderPlotly({
    req(input$boxplot_column)
    col_name <- input$boxplot_column
    
    plot_data <- filtered_data() %>%
      select(column = !!sym(col_name)) %>%
      filter(!is.na(column))
    
    if (!is.numeric(plot_data$column) || nrow(plot_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = paste(col_name, "is not a numeric variable for plotting or contains no data.")))
    }
    
    median_val <- median(plot_data$column, na.rm = TRUE)
    plot_title <- paste("Box Plot of", col_name)
    y_label <- col_name
    
    plot <- ggplot(plot_data, aes(y = column, x = 1)) +
      geom_boxplot(fill = "#f0ad4e", color = "#d9534f", outlier.color = "red", linewidth = 0.8) +
      
      geom_point(y = median_val, x = 1, color = "black", size = 4, shape = 18) +
      labs(
        title = plot_title,
        y = y_label,
        x = ""
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold", size = 16)
      )
    
    ggplotly(plot, tooltip = "y") %>%
      layout(
        yaxis = list(title = y_label),
        xaxis = list(showticklabels = FALSE, title = "")
      )
  })
  
  agg_data_plot1 <- reactive({
    req(input$selected_make)
    
    top_utilities <- filtered_data() %>%
      count(`Electric Utility`, sort = TRUE) %>%
      head(8) %>%
      pull(`Electric Utility`)
    
    selected_makes_filter <- input$selected_make
    if ("All" %in% selected_makes_filter || is.null(selected_makes_filter)) {
      top_n_makes <- filtered_data() %>%
        count(`Make`, sort = TRUE) %>%
        head(20) %>%
        pull(`Make`)
      selected_makes_filter <- top_n_makes
    }
    
    filtered_raw <- filtered_data() %>%
      filter(`Make` %in% selected_makes_filter,
             `Electric Utility` %in% top_utilities,
             `Model Year` >= start_year,  
             `Model Year` <= end_year   
      ) %>%
      mutate(electric_utility_nickname = recode(`Electric Utility`, !!!utility_nicknames))
    
    plot_data <- filtered_raw %>%
      group_by(`Model Year`, `Make`, electric_utility_nickname) %>%
      summarise(
        avg_range = mean(`Electric Range`, na.rm = TRUE),
        n_obs = n(),
        .groups = 'drop'
      ) %>%
      filter(n_obs > 1, !is.na(`Model Year`)) %>%
      
      filter(avg_range >= input$min_range)
    
    return(plot_data)
  })
  
  output$avg_range_box <- renderValueBox({
    data <- agg_data_plot1()
    avg_range <- mean(data$avg_range, na.rm = TRUE)
    valueBox(
      value = tags$p(paste(round(avg_range), "miles"), style = "font-size: 200%;"),
      subtitle = "Overall Average Range (2015-2019, Filtered Data)",
      icon = icon("gauge-high"),
      color = "green"
    )
  })
  
  output$max_range_box <- renderValueBox({
    data <- agg_data_plot1()
    max_range <- max(data$avg_range, na.rm = TRUE)
    valueBox(
      value = tags$p(paste(round(max_range), "miles"), style = "font-size: 200%;"),
      subtitle = "Highest Observed Average Range (2015-2019, Filtered Data)",
      icon = icon("car-bolt"),
      color = "blue"
    )
  })
  
  output$range_data_table <- renderDataTable({
    agg_data_plot1() %>%
      select(`Model Year`, `Make`, `Utility` = electric_utility_nickname, `Avg Range` = avg_range, `Count` = n_obs) %>%
      mutate(`Avg Range` = round(`Avg Range`, 1)) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  agg_data_plot_make <- reactive({
    agg_data_plot1() %>%
      group_by(`Model Year`, `Make`) %>%
      summarise(
        avg_range = mean(avg_range, na.rm = TRUE),
        .groups = 'drop'
      )
  })
  
  agg_data_plot_utility <- reactive({
    agg_data_plot1() %>%
      group_by(`Model Year`, electric_utility_nickname) %>%
      summarise(
        avg_range = mean(avg_range, na.rm = TRUE),
        .groups = 'drop'
      )
  })
  
  output$range_trend_make_plot <- renderPlot({
    plot_data <- agg_data_plot_make()
    req(nrow(plot_data) > 0)
    
    x_breaks <- seq(start_year, end_year, by = 1)
    
    plot_make <- ggplot(plot_data, aes(
      x = `Model Year`,
      y = avg_range,
      color = `Make`
    )) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = x_breaks, limits = c(start_year, end_year)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = "Average Range Trend by Vehicle Brand",
        x = "Model Year",
        y = "Average Electric Range (Miles)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    return(plot_make)
  })
  
  output$range_trend_utility_plot <- renderPlot({
    plot_data <- agg_data_plot_utility()
    req(nrow(plot_data) > 0)
    
    all_utility_order <- c(
      "PSE & Tacoma", "PSE", "Seattle & Tacoma", 
      "BPA & Clark County", "BPA & Tacoma & Peninsula",
      "PSE & Whatcom County", "BPA & Avista & Inland",
      "BPA & Snohomish County"
    )
    
    all_line_styles <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "22", "42")
    
    all_utility_colors <- c(
      "PSE & Tacoma" = "#E41A1C",
      "PSE" = "#377EB8",
      "Seattle & Tacoma" = "#4DAF4A",
      "BPA & Clark County" = "#984EA3",
      "BPA & Tacoma & Peninsula" = "#FF7F00",
      "PSE & Whatcom County" = "#A65628",
      "BPA & Avista & Inland" = "#F781BF",
      "BPA & Snohomish County" = "#999999"
    )
    
    x_breaks <- seq(start_year, end_year, by = 1)
    
    plot_utility <- ggplot(plot_data, aes(
      x = `Model Year`,
      y = avg_range,
      linetype = electric_utility_nickname,
      color = electric_utility_nickname
    )) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      
      scale_linetype_manual(
        name = "Electric Utility",
        values = all_line_styles,
        breaks = all_utility_order
      ) +
      
      scale_color_manual(
        name = "Electric Utility",
        values = all_utility_colors,
        breaks = all_utility_order
      ) + 
      
      scale_x_continuous(breaks = x_breaks, limits = c(start_year, end_year)) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
      labs(
        title = "Average Range Trend by Electric Utility",
        x = "Model Year",
        y = "Average Electric Range (Miles)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.width = unit(1.5, "cm")
      )
    
    return(plot_utility)
  })
  
  agg_data_plot2 <- reactive({
    req(input$top_n_counties, input$selected_type)
    n <- input$top_n_counties
    
    data_source <- filtered_data()
    
    county_totals <- data_source %>%
      filter(!is.na(`County`), `County` != "", `County` != "Other") %>% 
      count(`County`, sort = TRUE, name = "total_count") %>%
      head(n)
    
    top_n_counties <- county_totals$`County`
    
    plot_data <- data_source %>%
      filter(`County` %in% top_n_counties,
             `Electric Vehicle Type` %in% c("Battery Electric Vehicle (BEV)", "Plug-in Hybrid Electric Vehicle (PHEV)")) 
    
    if (input$selected_type != "All") {
      plot_data <- plot_data %>%
        filter(`Electric Vehicle Type` == input$selected_type)
    }
    
    plot_data <- plot_data %>%
      group_by(`County`, `Electric Vehicle Type`) %>%
      summarise(vehicle_count = n(), .groups = 'drop') %>%
      left_join(county_totals, by = "County") %>%
      mutate(`County` = factor(`County`, levels = county_totals$`County`))
    
    return(plot_data)
  })
  
  output$county_comp_plot <- renderPlotly({
    plot_data <- agg_data_plot2()
    req(nrow(plot_data) > 0)
    
    color_map <- c(
      "Battery Electric Vehicle (BEV)" = "#4daf4a",
      "Plug-in Hybrid Electric Vehicle (PHEV)" = "#377eb8"
    )
    
    plot2 <- ggplot(plot_data, aes(
      x = `County`,
      y = vehicle_count,
      fill = `Electric Vehicle Type`,
      text = paste0("County: ", `County`, "\nEV Type: ", `Electric Vehicle Type`, "\nCount: ", scales::comma(vehicle_count))
    )) +
      
      geom_bar(
        stat = "identity", 
        position = position_dodge(width = 0.8), 
        width = 0.7
      ) +
      
      scale_fill_manual(values = color_map, name = "EV Type") +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
      
      labs(
        title = paste("EV Population Composition: Top", input$top_n_counties, "Counties"),
        x = "County (Ranked by Total EV Count)",
        y = "Total Number of Electric Vehicles",
        fill = "Vehicle Type"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(plot2, tooltip = "text") %>%
      layout(margin = list(b = 100))
  })
}