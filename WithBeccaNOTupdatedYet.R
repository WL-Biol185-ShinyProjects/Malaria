
# ============================================================
# GLOBAL MALARIA RESEARCH PROJECT
# COMPLETE INTERACTIVE SHINY WEBSITE
# ============================================================

# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(leaflet)
library(readxl)
library(janitor)
library(htmltools)
library(scales)

# ============================================================
# 2. DOWNLOAD GLOBAL MALARIA DATA
# ============================================================

malaria <- read_csv(
  "https://ourworldindata.org/grapher/incidence-of-malaria.csv?v=1&csvType=full&useColumnShortNames=false",
  show_col_types = FALSE
)

entity_col <- names(malaria)[
  tolower(names(malaria)) == "entity"
][1]

code_col <- names(malaria)[
  tolower(names(malaria)) == "code"
][1]

year_col <- names(malaria)[
  tolower(names(malaria)) == "year"
][1]

value_col <- names(malaria)[
  grepl(
    "incidence of malaria",
    names(malaria),
    ignore.case = TRUE
  )
][1]

if (any(is.na(c(entity_col, code_col, year_col, value_col)))) {
  stop("A required malaria data column was not found.")
}

malaria_clean <- malaria %>%
  transmute(
    country = .data[[entity_col]],
    code = .data[[code_col]],
    year = as.integer(.data[[year_col]]),
    incidence = as.numeric(.data[[value_col]])
  ) %>%
  filter(year >= 2000, year <= 2024)

# ============================================================
# 3. DOWNLOAD WORLD MAP BOUNDARIES
# ============================================================

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

boundary_code <- if ("iso_a3_eh" %in% names(world)) {
  "iso_a3_eh"
} else {
  "iso_a3"
}

# ============================================================
# 4. GLOBAL MALARIA MAP COLORS
# ============================================================

my_colors <- c(
  "#f187d0",
  "#c2168c",
  "#890f63",
  "#312c54",
  "#0c0434"
)

color_domain <- range(
  malaria_clean$incidence,
  na.rm = TRUE,
  finite = TRUE
)

pal <- colorNumeric(
  palette = my_colors,
  domain = color_domain,
  na.color = "#d9d9d9"
)

# ============================================================
# 5. LOAD WHO THERAPEUTIC EFFICACY DATA
# ============================================================

who_xlsx <- "Becca/MTM_THERAPEUTIC_EFFICACY_STUDY_20261012.xlsx"
who_sheet <- "Data"
who_cache <- "Becca/who_clean.rds"

prepare_who <- function() {
  
  if (file.exists(who_cache)) {
    return(readRDS(who_cache))
  }
  
  if (!file.exists(who_xlsx)) {
    stop(
      "WHO Excel file not found: ",
      who_xlsx
    )
  }
  
  d <- read_excel(
    who_xlsx,
    sheet = who_sheet
  ) %>%
    clean_names()
  
  num_cols <- intersect(
    c(
      "latitude",
      "longitude",
      "year_start",
      "treatment_failure_pp",
      "treatment_failure_km",
      "positive_day_3_days"
    ),
    names(d)
  )
  
  d <- d %>%
    mutate(
      across(
        all_of(num_cols),
        ~ suppressWarnings(
          parse_number(as.character(.x))
        )
      )
    )
  
  text_cols <- intersect(
    c(
      "country_name",
      "site_name",
      "drug_name",
      "plasmodium_species"
    ),
    names(d)
  )
  
  d <- d %>%
    mutate(
      across(
        all_of(text_cols),
        ~ str_squish(as.character(.x))
      )
    )
  
  try({
    dir.create(
      dirname(who_cache),
      showWarnings = FALSE,
      recursive = TRUE
    )
    
    saveRDS(d, who_cache)
  }, silent = TRUE)
  
  d
}

who <- prepare_who()

required_cols <- c(
  "country_name",
  "site_name",
  "drug_name",
  "plasmodium_species",
  "year_start",
  "latitude",
  "longitude",
  "treatment_failure_pp"
)

missing_cols <- setdiff(
  required_cols,
  names(who)
)

if (length(missing_cols) > 0) {
  stop(
    "WHO dataset is missing columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

# ============================================================
# 6. PREPARE WHO DASHBOARD DATA
# ============================================================

species_choices <- sort(
  unique(na.omit(who$plasmodium_species))
)

default_species <- species_choices[
  str_detect(
    str_to_lower(species_choices),
    "falciparum"
  )
]

if (length(default_species) == 0) {
  default_species <- species_choices
}

drug_choices <- sort(
  unique(na.omit(who$drug_name))
)

country_choices <- sort(
  unique(na.omit(who$country_name))
)

valid_years <- who$year_start[
  is.finite(who$year_start)
]

if (length(valid_years) == 0) {
  stop("No valid WHO study years were found.")
}

year_range <- range(valid_years)

measure_choices <- c(
  "Per-protocol treatment failure (%)" =
    "treatment_failure_pp",
  "Kaplan-Meier treatment failure (%)" =
    "treatment_failure_km",
  "Day-3 positivity (%)" =
    "positive_day_3_days"
)

measure_choices <- measure_choices[
  measure_choices %in% names(who)
]

species_counts <- who %>%
  count(plasmodium_species, sort = TRUE)

drug_counts <- who %>%
  count(drug_name, sort = TRUE) %>%
  slice_head(n = 30)

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

# ============================================================
# 7. WEBSITE THEME
# ============================================================

website_theme <- bs_theme(
  version = 5,
  bootswatch = "sandstone",
  bg = "#312c54",
  fg = "#FFADE5",
  secondary = "#B193CC",
  success = "#890f63"
)


# ============================================================
# 8. DEFINE WEBSITE UI
# ============================================================

ui <- navbarPage(
  
  title = "Malaria | Past & Present",
  id = "main_nav",
  theme = website_theme,
  
  # ==========================================================
  # HOME PAGE
  # ==========================================================
  
  tabPanel(
    "Home Page",
    
    fluidPage(
      
      h1("Global Malaria Research Project"),
      
      p(
        "Welcome to the homepage. Use the navigation bar above ",
        "to explore the pathology, history, treatment failures, ",
        "and comparative case studies."
      ),
      
      hr(),
      
      h3("Global Malaria Incidence, 2000–2024"),
      
      p(
        "Explore the global distribution of malaria incidence ",
        "per 1,000 people at risk."
      ),
      
      sliderInput(
        "year",
        "Select a year:",
        min = 2000,
        max = 2024,
        value = 2006,
        step = 1,
        sep = "",
        width = "100%"
      ),
      
      div(
        style = "
          border: 3px solid #890f63;
          border-radius: 15px;
          padding: 8px;
          margin: 20px auto;
          max-width: 1100px;
          background-color: #312c54;
          overflow: hidden;
        ",
        
        leafletOutput(
          "map",
          height = "550px",
          width = "100%"
        )
      ),
      
      p(
        "Gray indicates missing data, not necessarily zero incidence.",
        style = "margin-top: 10px; font-style: italic;"
      )
    )
  ),
  
  # ==========================================================
  # WHAT IS MALARIA?
  # ==========================================================
  
  navbarMenu(
    
    "What is Malaria?",
    
    tabPanel(
      "Symptoms",
      
      fluidPage(
        
        h3("Clinical Symptoms of Malaria"),
        
        p(
          "Malaria is a life-threatening parasitic disease caused by ",
          "Plasmodium parasites and transmitted primarily through ",
          "the bites of infected female Anopheles mosquitoes. ",
          "Although malaria is preventable and curable, it continues ",
          "to pose a major global health challenge, particularly ",
          "in tropical and subtropical regions."
        ),
        
        p(
          "Five Plasmodium species are known to cause malaria in ",
          "humans: P. falciparum, P. vivax, P. malariae, P. ovale, ",
          "and P. knowlesi. P. falciparum is responsible for the ",
          "majority of malaria-related deaths."
        ),
        
        p(
          "Prevention strategies include insecticide-treated ",
          "mosquito nets, vector control, preventive medications, ",
          "and malaria vaccines. Early diagnosis and appropriate ",
          "antimalarial treatment are essential for reducing ",
          "complications and mortality."
        )
      )
    ),
    
    tabPanel(
      "Vector Transmission & Pathology",
      
      fluidPage(
        
        h3("Vector Transmission & Pathology"),
        
        p(
          "Details on the Anopheles mosquito vector, ",
          "Plasmodium life cycle, and human host pathology."
        )
      )
    ),
    
    tabPanel(
      "History of Malaria",
      
      fluidPage(
        
        h3("Historical Context"),
        
        p(
          "Timeline of global malaria discovery, historical impacts, ",
          "and past eradication milestones."
        )
      )
    ),
    
    tabPanel(
      "Location Frequency",
      
      fluidPage(
        
        h3("Global Location Frequency"),
        
        p(
          "Spatial distribution data, endemic zones, ",
          "and high-burden geographical tracking."
        )
      )
    )
  ),
  
  # ==========================================================
  # COMPARATIVE CASE STUDY
  # ==========================================================
  
  navbarMenu(
    
    "Comparative Case Study (2000-2015)",
    
    tabPanel(
      "WHO European Region Heat Map",
      
      fluidPage(
        
        h3("Indigenous Case Extinction Heat Map"),
        
        p(
          "Interactive heatmap or spatial data representing ",
          "the decline and eradication of malaria cases ",
          "across the European region between 2000 and 2015."
        )
      )
    ),
    
    tabPanel(
      "Azerbaijan Highlighted",
      
      fluidPage(
        
        h3("Focus: Azerbaijan Case Study"),
        
        p(
          "Detailed analysis of Azerbaijan's timeline ",
          "towards achieving malaria-free status."
        )
      )
    ),
    
    tabPanel(
      "Preventative Measures Comparison",
      
      fluidPage(
        
        h3("Preventative Measures & Infrastructure Comparison"),
        
        p(
          "Data visualization analyzing healthcare capacity indicators:"
        ),
        
        tags$ul(
          tags$li("Number of hospitals per population (#)"),
          tags$li("Number of physicians per population (#)")
        )
      )
    )
  ),
  
  # ==========================================================
  # TREATMENT
  # ==========================================================
  
  navbarMenu(
    
    "Treatment",
    
    tabPanel(
      "Treatments",
      
      fluidPage(
        
        h3("Current Standard Treatments"),
        
        p(
          "Overview of standard therapies, including ACTs ",
          "(Artemisinin-based combination therapies)."
        )
      )
    ),
    
    
    tabPanel(
      "Resistance & Mutation",
      
      fluidPage(
        
        h3("Zooming in on Resistance and Mutation"),
        
        p(
          "Deep dive into genetic mutations of Plasmodium strains ",
          "and molecular mechanisms behind drug resistance."
        )
      )
    ),
    
    # ========================================================
    # WHO THERAPEUTIC EFFICACY DASHBOARD
    # ========================================================
    
    tabPanel(
      "WHO Therapeutic Efficacy Dashboard",
      
      fluidPage(
        
        h2("WHO Therapeutic Efficacy Studies"),
        
        p(
          "Explore global antimalarial treatment efficacy, ",
          "treatment failure, drug performance, and study locations."
        ),
        
        sidebarLayout(
          
          sidebarPanel(
            
            width = 3,
            
            selectizeInput(
              "species",
              "Parasite species",
              choices = species_choices,
              selected = default_species,
              multiple = TRUE
            ),
            
            selectizeInput(
              "drugs",
              "Drug",
              choices = drug_choices,
              multiple = TRUE,
              options = list(
                placeholder = "All drugs"
              )
            ),
            
            selectizeInput(
              "countries",
              "Country",
              choices = country_choices,
              multiple = TRUE,
              options = list(
                placeholder = "All countries"
              )
            ),
            
            sliderInput(
              "years",
              "Study start year",
              min = year_range[1],
              max = year_range[2],
              value = year_range,
              step = 1,
              sep = ""
            ),
            
            selectInput(
              "measure",
              "Outcome shown on maps",
              choices = measure_choices
            ),
            
            sliderInput(
              "cap",
              "Color scale maximum (%)",
              min = 10,
              max = 100,
              value = 50,
              step = 5
            ),
            
            checkboxInput(
              "crop",
              "Crop static maps to study region",
              value = TRUE
            ),
            
            hr(),
            
            textOutput("n_shown"),
            
            helpText(
              "Records with a missing start year are excluded. ",
              "Each row represents a WHO database record, not ",
              "necessarily a separate study."
            )
          ),
          
          mainPanel(
            
            width = 9,
            
            tabsetPanel(
              
              tabPanel(
                "WHO Study Locations",
                br(),
                leafletOutput("leaf", height = "600px")
              ),
              
              tabPanel(
                "Treatment Outcome Map",
                br(),
                plotOutput("plot_outcome", height = "560px"),
                br(),
                downloadButton("dl_outcome", "Download PNG")
              ),
              
              tabPanel(
                "Study Locations",
                br(),
                plotOutput("plot_sites", height = "560px"),
                br(),
                downloadButton("dl_sites", "Download PNG")
              ),
              
              tabPanel(
                "Studies by Year",
                br(),
                plotOutput("plot_years", height = "450px"),
                br(),
                downloadButton("dl_years", "Download PNG")
              ),
              
              tabPanel(
                "Drug Summary",
                br(),
                tableOutput("drug_summary"),
                br(),
                downloadButton("dl_summary", "Download CSV")
              ),
              
              tabPanel(
                "Dataset Overview",
                br(),
                
                h4("Parasite Species (All Records)"),
                
                tableOutput("species_table"),
                
                h4("Most Common Drugs (All Records)"),
                
                tableOutput("drug_table")
              )
            )
          )
        )
      )
    )
  ),
  
  # ==========================================================
  # FINAL REMARKS
  # ==========================================================
  
  tabPanel(
    
    "Final Remarks",
    
    fluidPage(
      
      h3("Resources"),
      
      p(
        "Additional helpful external links, databases, and toolkits."
      ),
      
      hr(),
      
      h3("Works Cited"),
      
      p(
        "Academic bibliography and data sources."
      ),
      
      hr(),
      
      h3("Additional Avenues"),
      
      p(
        "Future directions for malaria research, vaccine updates, ",
        "and emerging vector control methods."
      )
    )
  ),
  
  # ==========================================================
  # ABOUT US
  # ==========================================================
  
  tabPanel(
    
    "About Us",
    
    fluidPage(
      
      h3("About the Research Team"),
      
      p(
        "Project background, contributor roles, ",
        "and institutional affiliations."
      )
    )
  )
)


# ============================================================
# 9. DEFINE SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ==========================================================
  # GLOBAL MALARIA INCIDENCE MAP
  # ==========================================================
  
  output$map <- renderLeaflet({
    
    leaflet(world) %>%
      addTiles() %>%
      setView(
        lng = 15,
        lat = 15,
        zoom = 2
      )
  })
  
  observeEvent(input$year, {
    
    year_data <- malaria_clean %>%
      filter(
        year == input$year,
        !is.na(code),
        code != ""
      ) %>%
      select(code, incidence) %>%
      distinct(code, .keep_all = TRUE)
    
    world_year <- world %>%
      left_join(
        year_data,
        by = setNames("code", boundary_code)
      )
    
    leafletProxy(
      "map",
      data = world_year
    ) %>%
      
      clearGroup("countries") %>%
      removeControl("legend") %>%
      
      addPolygons(
        group = "countries",
        fillColor = ~pal(incidence),
        fillOpacity = 0.8,
        color = "white",
        weight = 0.5,
        
        label = ~paste0(
          name,
          ": ",
          ifelse(
            is.na(incidence),
            "No data",
            paste0(
              round(incidence, 2),
              " per 1,000"
            )
          )
        ),
        
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#333333",
          bringToFront = TRUE
        )
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = color_domain,
        title = paste0(
          "Incidence per 1,000 at risk — ",
          input$year
        ),
        layerId = "legend"
      )
    
  }, ignoreInit = FALSE)
  
  # ==========================================================
  # WHO DASHBOARD: FILTERED DATA
  # ==========================================================
  
  filtered <- reactive({
    
    req(input$species)
    
    d <- who %>%
      filter(
        plasmodium_species %in% input$species,
        !is.na(year_start),
        between(
          year_start,
          input$years[1],
          input$years[2]
        )
      )
    
    if (length(input$drugs) > 0) {
      d <- d %>%
        filter(drug_name %in% input$drugs)
    }
    
    if (length(input$countries) > 0) {
      d <- d %>%
        filter(country_name %in% input$countries)
    }
    
    d
  })
  
  # ==========================================================
  # WHO DASHBOARD: GEOGRAPHIC DATA
  # ==========================================================
  
  geo <- reactive({
    
    filtered() %>%
      filter(
        !is.na(latitude),
        !is.na(longitude),
        between(latitude, -90, 90),
        between(longitude, -180, 180)
      )
  })
  
  outcome_data <- reactive({
    
    m <- input$measure
    
    geo() %>%
      filter(
        !is.na(.data[[m]]),
        between(.data[[m]], 0, 100)
      ) %>%
      mutate(
        value = .data[[m]]
      ) %>%
      arrange(value)
  })
  
  measure_label <- reactive({
    
    names(measure_choices)[
      measure_choices == input$measure
    ]
  })
  
  # ==========================================================
  # WHO DASHBOARD: RECORD COUNTS
  # ==========================================================
  
  output$n_shown <- renderText({
    
    sprintf(
      "%s of %s records match the filters; %s have map coordinates.",
      format(nrow(filtered()), big.mark = ","),
      format(nrow(who), big.mark = ","),
      format(nrow(geo()), big.mark = ",")
    )
  })
  
  # ==========================================================
  # WHO DASHBOARD: INTERACTIVE MAP
  # ==========================================================
  
  output$leaf <- renderLeaflet({
    
    d <- outcome_data()
    
    validate(
      need(
        nrow(d) > 0,
        "No records with coordinates and this outcome match the filters."
      )
    )
    
    cap <- input$cap
    
    who_pal <- colorNumeric(
      palette = "viridis",
      domain = c(0, cap),
      na.color = "#808080"
    )
    
    lab <- measure_label()
    
    leaflet(d) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~who_pal(pmin(value, cap)),
        fillOpacity = 0.75,
        stroke = TRUE,
        weight = 1,
        
        popup = ~paste0(
          "<b>Country:</b> ",
          htmlEscape(as.character(country_name)),
          "<br><b>Study site:</b> ",
          htmlEscape(as.character(site_name)),
          "<br><b>Year:</b> ",
          htmlEscape(as.character(year_start)),
          "<br><b>Drug:</b> ",
          htmlEscape(as.character(drug_name)),
          "<br><b>Species:</b> ",
          htmlEscape(as.character(plasmodium_species)),
          "<br><b>",
          htmlEscape(lab),
          ":</b> ",
          round(value, 1),
          "%"
        )
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = who_pal,
        values = c(0, cap),
        title = lab,
        labFormat = labelFormat(suffix = "%")
      )
  })
  
  # ==========================================================
  # WHO DASHBOARD: TREATMENT OUTCOME MAP
  # ==========================================================
  
  plot_outcome <- reactive({
    
    d <- outcome_data()
    
    validate(
      need(
        nrow(d) > 0,
        "No records match the selected filters."
      )
    )
    
    cap <- input$cap
    lab <- measure_label()
    
    ggplot() +
      
      geom_sf(
        data = world,
        fill = "grey95",
        color = "grey75",
        linewidth = 0.2
      ) +
      
      geom_point(
        data = d,
        aes(
          x = longitude,
          y = latitude,
          color = value
        ),
        size = 2,
        alpha = 0.75
      ) +
      
      scale_color_viridis_c(
        option = "C",
        limits = c(0, cap),
        oob = scales::squish,
        name = lab
      ) +
      
      coord_sf(
        xlim = if (input$crop) crop_x,
        ylim = if (input$crop) crop_y,
        expand = FALSE,
        default_crs = st_crs(4326)
      ) +
      
      labs(
        title = paste(
          "Global distribution:",
          tolower(lab)
        ),
        subtitle = paste0(
          "WHO therapeutic efficacy studies; values above ",
          cap,
          "% shown in the top color"
        ),
        caption = "Source: WHO therapeutic efficacy study database.",
        x = NULL,
        y = NULL
      ) +
      
      map_theme
  })
  
  output$plot_outcome <- renderPlot({
    plot_outcome()
  })
  
  # ==========================================================
  # WHO DASHBOARD: STUDY LOCATIONS
  # ==========================================================
  
  plot_sites <- reactive({
    
    d <- geo() %>%
      count(
        longitude,
        latitude,
        name = "number_of_records"
      ) %>%
      arrange(desc(number_of_records))
    
    validate(
      need(
        nrow(d) > 0,
        "No records with coordinates match the filters."
      )
    )
    
    ggplot() +
      
      geom_sf(
        data = world,
        fill = "grey95",
        color = "grey75",
        linewidth = 0.2
      ) +
      
      geom_point(
        data = d,
        aes(
          x = longitude,
          y = latitude,
          size = number_of_records
        ),
        color = "firebrick",
        alpha = 0.55
      ) +
      
      scale_size_area(
        max_size = 7,
        breaks = whole_breaks,
        name = "Records at coordinate"
      ) +
      
      coord_sf(
        xlim = if (input$crop) crop_x,
        ylim = if (input$crop) crop_y,
        expand = FALSE,
        default_crs = st_crs(4326)
      ) +
      
      labs(
        title = "Global distribution of malaria efficacy studies",
        subtitle = "Circle size represents the number of records at each coordinate",
        caption = "Source: WHO therapeutic efficacy study database",
        x = NULL,
        y = NULL
      ) +
      
      map_theme
  })
  
  output$plot_sites <- renderPlot({
    plot_sites()
  })
  
  # ==========================================================
  # WHO DASHBOARD: STUDIES BY YEAR
  # ==========================================================
  
  plot_years <- reactive({
    
    d <- filtered() %>%
      count(year_start)
    
    validate(
      need(
        nrow(d) > 0,
        "No records match the filters."
      )
    )
    
    ggplot(
      d,
      aes(
        x = year_start,
        y = n
      )
    ) +
      
      geom_col(
        fill = "#890f63"
      ) +
      
      labs(
        title = "WHO therapeutic efficacy studies by year",
        x = "Study start year",
        y = "Number of records"
      ) +
      
      theme_minimal(base_size = 12)
  })
  
  output$plot_years <- renderPlot({
    plot_years()
  })
  
  # ==========================================================
  # WHO DASHBOARD: DRUG SUMMARY
  # ==========================================================
  
  drug_summary <- reactive({
    
    d <- filtered()
    
    validate(
      need(
        nrow(d) > 0,
        "No records match the filters."
      )
    )
    
    result <- d %>%
      group_by(drug_name) %>%
      summarise(
        records = n(),
        countries = n_distinct(country_name),
        
        across(
          any_of(
            c(
              "treatment_failure_pp",
              "treatment_failure_km",
              "positive_day_3_days"
            )
          ),
          ~ if (all(is.na(.x))) {
            NA_real_
          } else {
            median(.x, na.rm = TRUE)
          },
          .names = "median_{.col}"
        ),
        
        .groups = "drop"
      )
    
    if ("median_treatment_failure_pp" %in% names(result)) {
      result <- result %>%
        arrange(desc(median_treatment_failure_pp))
    }
    
    result
  })
  
  output$drug_summary <- renderTable({
    drug_summary()
  }, digits = 1, na = "")
  
  # ==========================================================
  # WHO DASHBOARD: OVERVIEW TABLES
  # ==========================================================
  
  output$species_table <- renderTable({
    
    species_counts %>%
      rename(
        Species = plasmodium_species,
        Records = n
      )
  })
  
  output$drug_table <- renderTable({
    
    drug_counts %>%
      rename(
        Drug = drug_name,
        Records = n
      )
  })
  
  # ==========================================================
  # WHO DASHBOARD: DOWNLOAD FUNCTIONS
  # ==========================================================
  
  save_png <- function(p, w, h) {
    
    function(file) {
      
      ggsave(
        file,
        plot = p(),
        width = w,
        height = h,
        units = "in",
        dpi = 300
      )
    }
  }
  
  output$dl_outcome <- downloadHandler(
    
    filename = function() {
      "treatment_outcome_map.png"
    },
    
    content = save_png(
      plot_outcome,
      12,
      6
    )
  )
  
  output$dl_sites <- downloadHandler(
    
    filename = function() {
      "study_locations_map.png"
    },
    
    content = save_png(
      plot_sites,
      12,
      6
    )
  )
  
  output$dl_years <- downloadHandler(
    
    filename = function() {
      "studies_by_year.png"
    },
    
    content = save_png(
      plot_years,
      9,
      5
    )
  )
  
  output$dl_summary <- downloadHandler(
    
    filename = function() {
      "drug_summary.csv"
    },
    
    content = function(file) {
      write_csv(
        drug_summary(),
        file
      )
    }
  )
  
}

# ============================================================
# 10. LAUNCH COMPLETE WEBSITE
# ============================================================

shinyApp(
  ui = ui,
  server = server
)

