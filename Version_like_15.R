
# ============================================================
# GLOBAL MALARIA RESEARCH PROJECT
# Complete Interactive Shiny Website
# ============================================================
# 1. LOAD PACKAGES
library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(leaflet)
# ============================================================
# 2. DOWNLOAD AND PREPARE MALARIA DATA
# ============================================================
malaria <- read_csv(
  "https://ourworldindata.org/grapher/incidence-of-malaria.csv?v=1&csvType=full&useColumnShortNames=false",
  show_col_types = FALSE
)
# Identify relevant columns
entity_col <- names(malaria)[tolower(names(malaria)) == "entity"][1]
code_col <- names(malaria)[tolower(names(malaria)) == "code"][1]
year_col <- names(malaria)[tolower(names(malaria)) == "year"][1]
value_col <- names(malaria)[grepl("incidence of malaria", names(malaria), ignore.case = TRUE)][1]
# Check required columns
if (any(is.na(c(entity_col, code_col, year_col, value_col)))) {
  stop("A required malaria data column was not found. Check names(malaria).")
}
# Clean malaria data
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
# Identify country code column
boundary_code <- if ("iso_a3_eh" %in% names(world)) {
  "iso_a3_eh"
} else {
  "iso_a3"
}
# ============================================================
# 4. DEFINE MAP COLORS
# ============================================================
my_colors <- c(
  "#f187d0",
  "#c2168c",
  "#890f63",
  "#312c54",
  "#0c0434"
)
# Keep color scale consistent across years
color_domain <- range(malaria_clean$incidence, na.rm = TRUE, finite = TRUE)
pal <- colorNumeric(
  palette = my_colors,
  domain = color_domain,
  na.color = "#d9d9d9"
)
# ============================================================
# 5. DEFINE WEBSITE UI
# ============================================================
ui <- navbarPage(
  title = "Malaria | Past & Present",
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "sandstone",
    bg = "#312c54",
    fg = "#FFADE5",
    secondary = "#B193CC",
    success = "#890f63"
  ),
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
        inputId = "year",
        label = "Select a year:",
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
          outputId = "map",
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
  # SECTION 1: WHAT IS MALARIA?
  # ==========================================================
  navbarMenu(
    "What is Malaria?",
    tabPanel(
      "Symptoms",
      fluidPage(
        h3("Clinical Symptoms of Malaria"),
        p(
          "Malaria is a life-threatening parasitic disease caused by Plasmodium parasites and transmitted primarily through the bites of infected female Anopheles mosquitoes. Although malaria is preventable and curable, it continues to pose a major global health challenge, particularly in tropical and subtropical regions. Malaria is not directly contagious between humans; transmission occurs primarily through infected mosquitoes, although rare cases can occur through blood transfusions, organ transplantation, or congenital transmission.

Five Plasmodium species are known to cause malaria in humans: P. falciparum, P. vivax, P. malariae, P. ovale, and P. knowlesi. Among these, P. falciparum is responsible for the majority of malaria-related deaths and is especially prevalent in sub-Saharan Africa. P. vivax is the most geographically widespread species outside sub-Saharan Africa and is notable for its ability to cause relapses. The remaining species also contribute to the global burden of malaria, with varying distributions and clinical presentations.

Although malaria can cause severe illness and death, it is both preventable and curable. Prevention strategies include insecticide-treated mosquito nets, vector control, preventive medications, and malaria vaccines. Early diagnosis and appropriate antimalarial treatment are essential for reducing complications and mortality. Despite these advances, drug resistance, limited healthcare access, and socioeconomic barriers continue to challenge global malaria control efforts.
"
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
  # SECTION 2: COMPARATIVE CASE STUDY
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
  # SECTION 3: TREATMENT
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
      "Failed Treatments (4 Reasons)",
      fluidPage(
        h3("Failed Treatments"),
        p(
          "Analysis of historically failed treatment frameworks ",
          "due to these four major factors:"
        ),
        tags$ol(
          tags$li("Inadequate dosage or poor patient compliance."),
          tags$li("Counterfeit or sub-standard drug quality."),
          tags$li("Lack of healthcare infrastructure or supply chain breakdowns."),
          tags$li("Inappropriate self-treatment without proper diagnostics.")
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
    )
  ),
  
  
  # ==========================================================
  # SECTION 4: Final Remarks & Cited Works
  # ==========================================================
  tabPanel(
    "Final Remarks",
    fluidPage(
      h3("Resources"),
      p("Additional helpful external links, databases, and toolkits."),
      hr(),
      h3("Works Cited"),
      p("Academic bibliography and data sources."),
      hr(),
      h3("Additional Avenues"),
      p(
        "Future directions for malaria research, vaccine updates, ",
        "and emerging vector control methods."
      )
    )
  ),

  # ============================================================
  # SECTION 5: ABOUT US
  # ============================================================
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
# 6. DEFINE SERVER
# ============================================================
server <- function(input, output, session) {
  # Initialize interactive map
  output$map <- renderLeaflet({
    leaflet(world) %>%
      addTiles() %>%
      setView(lng = 15, lat = 15, zoom = 2)
  })
  # Update map when user changes year
  observeEvent(input$year, {
    # Select malaria incidence for selected year
    year_data <- malaria_clean %>%
      filter(
        year == input$year,
        !is.na(code),
        code != ""
      ) %>%
      select(code, incidence) %>%
      distinct(code, .keep_all = TRUE)
    # Join malaria data with country boundaries
    world_year <- world %>%
      left_join(
        year_data,
        by = setNames("code", boundary_code)
      )
    # Update map without resetting zoom
    leafletProxy("map", data = world_year) %>%
      clearGroup("countries") %>%
      removeControl("legend") %>%
      addPolygons(
        group = "countries",
        fillColor = ~pal(incidence),
        fillOpacity = 0.8,
        color = "white",
        weight = 0.5,
        # Country hover labels
        label = ~paste0(
          name, ": ",
          ifelse(
            is.na(incidence),
            "No data",
            paste0(round(incidence, 2), " per 1,000")
          )
        ),
        # Highlight countries when hovered over
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#333333",
          bringToFront = TRUE
        )
      ) %>%
      # Add map legend
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
}
# ============================================================
# 7. LAUNCH COMPLETE WEBSITE
# ============================================================
shinyApp(
  ui = ui,
  server = server
)