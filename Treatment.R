
# WHO therapeutic efficacy studies: Shiny app

library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(rnaturalearth)
library(leaflet)
library(htmltools)


# 1. Load and clean the data once, when the app starts
who_xlsx  <- "Becca/MTM_THERAPEUTIC_EFFICACY_STUDY_20261012.xlsx"
who_sheet <- "Data"
who_cache <- "Becca/who_clean.rds"   # fast-loading copy of the cleaned data

prepare_who <- function() {
  if (file.exists(who_cache)) return(readRDS(who_cache))
  
  if (!file.exists(who_xlsx)) {
    stop("Data file not found. Put the WHO Excel file at: ", who_xlsx)
  }
  
  d <- read_excel(who_xlsx, sheet = who_sheet) |>
    clean_names()
  
  # Columns that should be numeric (they often import as text)
  num_cols <- intersect(
    c("latitude", "longitude", "year_start",
      "treatment_failure_pp", "treatment_failure_km", "positive_day_3_days"),
    names(d)
  )
  d <- d |>
    mutate(across(all_of(num_cols),
                  ~ suppressWarnings(parse_number(as.character(.x)))))
  
  text_cols <- intersect(
    c("country_name", "site_name", "drug_name", "plasmodium_species"),
    names(d)
  )
  d <- d |>
    mutate(across(all_of(text_cols), ~ str_squish(as.character(.x))))
  
  # Save a fast copy for next time (ignored if the folder is read-only)
  try({
    dir.create(dirname(who_cache), showWarnings = FALSE, recursive = TRUE)
    saveRDS(d, who_cache)
  }, silent = TRUE)
  
  d
}

who <- prepare_who()

required_cols <- c("country_name", "site_name", "drug_name", "plasmodium_species",
                   "year_start", "latitude", "longitude", "treatment_failure_pp")
missing_cols <- setdiff(required_cols, names(who))
if (length(missing_cols) > 0) {
  stop("The data file is missing columns: ", paste(missing_cols, collapse = ", "),
       "\nCheck the column names after clean_names().")
}

#2. Things computed once
world <- ne_countries(scale = "medium", returnclass = "sf")

species_choices <- sort(unique(na.omit(who$plasmodium_species)))
default_species <- species_choices[str_detect(str_to_lower(species_choices), "falciparum")]
if (length(default_species) == 0) default_species <- species_choices

drug_choices    <- sort(unique(na.omit(who$drug_name)))
country_choices <- sort(unique(na.omit(who$country_name)))
year_range      <- range(who$year_start, na.rm = TRUE)

measure_choices <- c(
  "Per-protocol treatment failure (%)" = "treatment_failure_pp",
  "Kaplan-Meier treatment failure (%)" = "treatment_failure_km",
  "Day-3 positivity (%)"               = "positive_day_3_days"
)
measure_choices <- measure_choices[measure_choices %in% names(who)]

# Overview tables for the whole dataset
species_counts <- who |> count(plasmodium_species, sort = TRUE)
drug_counts    <- who |> count(drug_name, sort = TRUE) |> slice_head(n = 30)

crop_x <- c(-90, 165)
crop_y <- c(-35, 42)

whole_breaks <- function(x) {
  b <- pretty(x, n = 4)
  b[b == round(b) & b >= 1]
}

map_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10),
    legend.position = "right",
    plot.title.position = "plot",
    plot.margin = margin(15, 20, 15, 15)
  )


#3. User interface 
ui <- fluidPage(
  titlePanel("Treatment Outcomes"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput("species", "Parasite species",
                     choices = species_choices, selected = default_species,
                     multiple = TRUE),
      selectizeInput("drugs", "Drug", choices = drug_choices, multiple = TRUE,
                     options = list(placeholder = "All drugs")),
      selectizeInput("countries", "Country", choices = country_choices,
                     multiple = TRUE,
                     options = list(placeholder = "All countries")),
      sliderInput("years", "Study start year",
                  min = year_range[1], max = year_range[2],
                  value = year_range, step = 1, sep = ""),
      selectInput("measure", "Outcome shown on the maps",
                  choices = measure_choices),
      sliderInput("cap", "Color scale maximum (%)",
                  min = 10, max = 100, value = 50, step = 5),
      checkboxInput("crop", "Crop static maps to the study region", value = TRUE),
      hr(),
      textOutput("n_shown"),
      helpText("Records with a missing start year are excluded. ",
               "Each record is a row in the WHO database, not necessarily a separate study.")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("WHO therapeutic efficacy studies", br(),
                 leafletOutput("leaf", height = 600)),
        
        tabPanel("Treatment outcome map", br(),
                 plotOutput("plot_outcome", height = 560),
                 br(), downloadButton("dl_outcome", "Download PNG")),
        
        tabPanel("Study locations", br(),
                 plotOutput("plot_sites", height = 560),
                 br(), downloadButton("dl_sites", "Download PNG")),
        
        tabPanel("Studies by year", br(),
                 plotOutput("plot_years", height = 450),
                 br(), downloadButton("dl_years", "Download PNG")),
        
        tabPanel("Drug summary", br(),
                 tableOutput("drug_summary"),
                 br(), downloadButton("dl_summary", "Download CSV")),
        
        tabPanel("Dataset overview", br(),
                 h4("Parasite species (all records)"),
                 tableOutput("species_table"),
                 h4("Most common drugs (all records)"),
                 tableOutput("drug_table"))
      )
    )
  )
)


# 4. Server
server <- function(input, output, session) {
  
  # Records matching the sidebar filters
  filtered <- reactive({
    req(input$species)
    d <- who |>
      filter(plasmodium_species %in% input$species,
             !is.na(year_start),
             between(year_start, input$years[1], input$years[2]))
    if (length(input$drugs) > 0)     d <- d |> filter(drug_name %in% input$drugs)
    if (length(input$countries) > 0) d <- d |> filter(country_name %in% input$countries)
    d
  })
  
  # Records with usable coordinates
  geo <- reactive({
    filtered() |>
      filter(!is.na(latitude), !is.na(longitude),
             between(latitude, -90, 90), between(longitude, -180, 180))
  })
  
  # Records with a usable value for the selected outcome
  outcome_data <- reactive({
    m <- input$measure
    geo() |>
      filter(!is.na(.data[[m]]), between(.data[[m]], 0, 100)) |>
      mutate(value = .data[[m]]) |>
      arrange(value)              # lowest first, so the highest draw on top
  })
  
  measure_label <- reactive({
    names(measure_choices)[measure_choices == input$measure]
  })
  
  output$n_shown <- renderText({
    sprintf("%s of %s records match the filters; %s have map coordinates.",
            format(nrow(filtered()), big.mark = ","),
            format(nrow(who), big.mark = ","),
            format(nrow(geo()), big.mark = ","))
  })
  
  # ---- Interactive map ----
  output$leaf <- renderLeaflet({
    d <- outcome_data()
    validate(need(nrow(d) > 0, "No records with coordinates and this outcome match the filters."))
    
    cap <- input$cap
    pal <- colorNumeric(palette = "viridis", domain = c(0, cap), na.color = "#808080")
    lab <- measure_label()
    
    leaflet(d) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~pal(pmin(value, cap)),   # values above the cap get the top color
        fillOpacity = 0.75,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0(
          "<b>Country:</b> ",      htmlEscape(as.character(country_name)),
          "<br><b>Study site:</b> ", htmlEscape(as.character(site_name)),
          "<br><b>Year:</b> ",     htmlEscape(as.character(year_start)),
          "<br><b>Drug:</b> ",     htmlEscape(as.character(drug_name)),
          "<br><b>Species:</b> ",  htmlEscape(as.character(plasmodium_species)),
          "<br><b>", htmlEscape(lab), ":</b> ", round(value, 1), "%"
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = c(0, cap),
        title = lab,
        labFormat = labelFormat(suffix = "%")
      )
  })
  
  # ---- Static outcome map ----
  plot_outcome <- reactive({
    d <- outcome_data()
    validate(need(nrow(d) > 0, "No records with coordinates and this outcome match the filters."))
    cap <- input$cap
    lab <- measure_label()
    
    ggplot() +
      geom_sf(data = world, fill = "grey95", color = "grey75", linewidth = 0.2) +
      geom_point(data = d,
                 aes(x = longitude, y = latitude, color = value),
                 size = 2, alpha = 0.75) +
      scale_color_viridis_c(option = "C", limits = c(0, cap),
                            oob = scales::squish, name = lab) +
      coord_sf(xlim = if (input$crop) crop_x, ylim = if (input$crop) crop_y,
               expand = FALSE, default_crs = st_crs(4326)) +
      labs(title = paste("Global distribution:", tolower(lab)),
           subtitle = paste0("WHO therapeutic efficacy studies; values above ", cap,
                             "% shown in the top color"),
           caption = "Source: WHO therapeutic efficacy study database. Each point is one study record.",
           x = NULL, y = NULL) +
      map_theme
  })
  output$plot_outcome <- renderPlot(plot_outcome())
  
  # ---- Study locations ----
  plot_sites <- reactive({
    d <- geo() |>
      count(longitude, latitude, name = "number_of_records") |>
      arrange(desc(number_of_records))
    validate(need(nrow(d) > 0, "No records with coordinates match the filters."))
    
    ggplot() +
      geom_sf(data = world, fill = "grey95", color = "grey75", linewidth = 0.2) +
      geom_point(data = d,
                 aes(x = longitude, y = latitude, size = number_of_records),
                 color = "firebrick", alpha = 0.55) +
      scale_size_area(max_size = 7, breaks = whole_breaks,
                      name = "Records at\ncoordinate") +
      coord_sf(xlim = if (input$crop) crop_x, ylim = if (input$crop) crop_y,
               expand = FALSE, default_crs = st_crs(4326)) +
      labs(title = "Global distribution of malaria efficacy studies",
           subtitle = "Circle size represents the number of records at each coordinate",
           caption = "Source: WHO therapeutic efficacy study database",
           x = NULL, y = NULL) +
      map_theme
  })
  output$plot_sites <- renderPlot(plot_sites())
  
  # ---- Studies by year ----
  plot_years <- reactive({
    d <- filtered() |> count(year_start)
    validate(need(nrow(d) > 0, "No records match the filters."))
    
    ggplot(d, aes(x = year_start, y = n)) +
      geom_col() +
      labs(title = "WHO therapeutic efficacy studies by year",
           x = "Study start year", y = "Number of records") +
      theme_minimal(base_size = 12)
  })
  output$plot_years <- renderPlot(plot_years())
  
  # ---- Drug summary ----
  drug_summary <- reactive({
    d <- filtered()
    validate(need(nrow(d) > 0, "No records match the filters."))
    
    d |>
      group_by(drug_name) |>
      summarise(
        records   = n(),
        countries = n_distinct(country_name),
        across(any_of(c("treatment_failure_pp", "treatment_failure_km",
                        "positive_day_3_days")),
               ~ median(.x, na.rm = TRUE), .names = "median_{.col}"),
        .groups = "drop"
      ) |>
      arrange(desc(median_treatment_failure_pp)) |>
      rename(any_of(c(
        "Drug" = "drug_name",
        "Records" = "records",
        "Countries" = "countries",
        "Median failure, per-protocol (%)" = "median_treatment_failure_pp",
        "Median failure, Kaplan-Meier (%)" = "median_treatment_failure_km",
        "Median day-3 positivity (%)" = "median_positive_day_3_days"
      )))
  })
  output$drug_summary <- renderTable(drug_summary(), digits = 1, na = "")
  
  # ---- Overview tables ----
  output$species_table <- renderTable(
    species_counts |> rename(Species = plasmodium_species, Records = n))
  output$drug_table <- renderTable(
    drug_counts |> rename(Drug = drug_name, Records = n))
  
  # ---- Downloads ----
  save_png <- function(p, w, h) {
    function(file) ggsave(file, plot = p(), width = w, height = h,
                          units = "in", dpi = 300)
  }
  output$dl_outcome <- downloadHandler("treatment_outcome_map.png",
                                       save_png(plot_outcome, 12, 6))
  output$dl_sites   <- downloadHandler("study_locations_map.png",
                                       save_png(plot_sites, 12, 6))
  output$dl_years   <- downloadHandler("studies_by_year.png",
                                       save_png(plot_years, 9, 5))
  output$dl_summary <- downloadHandler(
    "drug_summary.csv",
    function(file) write_csv(drug_summary(), file)
  )
}

shinyApp(ui, server)
