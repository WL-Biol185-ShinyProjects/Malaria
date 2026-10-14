
# Run this installation line ONCE if you need these packages
install.packages(c(
  "shiny", "tidyverse", "sf", "rnaturalearth",
  "rnaturalearthdata", "leaflet"
))

# Load packages
library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(leaflet)

# Download malaria incidence data from Our World in Data
malaria <- read_csv(
  "https://ourworldindata.org/grapher/incidence-of-malaria.csv?v=1&csvType=full&useColumnShortNames=false",
  show_col_types = FALSE
)

# Identify the relevant columns
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
  grepl("incidence of malaria", names(malaria),
        ignore.case = TRUE)
][1]

# Check that all required columns were found
if (any(is.na(c(entity_col, code_col, year_col, value_col)))) {
  stop("A required column was not found. Run names(malaria) to inspect the data.")
}

# Clean and standardize the data
malaria_clean <- malaria %>%
  transmute(
    country = .data[[entity_col]],
    code = .data[[code_col]],
    year = as.integer(.data[[year_col]]),
    incidence = as.numeric(.data[[value_col]])
  ) %>%
  filter(year >= 2000, year <= 2024)

# Download country boundaries
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Choose the country-code column
boundary_code <- if ("iso_a3_eh" %in% names(world)) {
  "iso_a3_eh"
} else {
  "iso_a3"
}

# Custom pink and purple palette
my_colors <- c(
  "#f187d0",
  "#c2168c",
  "#890f63",
  "#312c54",
  "#0c0434"
)

# Keep the same color scale for every year
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

# User interface
ui <- fluidPage(
  titlePanel("Global Malaria Incidence, 2000–2024"),
  
  p("Malaria incidence per 1,000 people at risk."),
  
  sliderInput(
    inputId = "year",
    label = "Select a year:",
    min = 2000,
    max = 2024,
    value = 2006,
    step = 1,
    sep = ""
  ),
  
  leafletOutput("map", height = "650px"),
  
  p(
    "Gray indicates missing data, not necessarily zero incidence."
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Draw the base map once
  output$map <- renderLeaflet({
    leaflet(world) %>%
      addTiles() %>%
      setView(lng = 15, lat = 15, zoom = 2)
  })
  
  # Update country colors whenever the year changes
  observeEvent(input$year, {
    
    # Select the incidence data for the chosen year
    year_data <- malaria_clean %>%
      filter(
        year == input$year,
        !is.na(code),
        code != ""
      ) %>%
      select(code, incidence) %>%
      distinct(code, .keep_all = TRUE)
    
    # Join incidence values to country boundaries
    world_year <- world %>%
      left_join(
        year_data,
        by = setNames("code", boundary_code)
      )
    
    # Update the map without resetting the zoom
    leafletProxy("map", data = world_year) %>%
      clearGroup("countries") %>%
      removeControl("legend") %>%
      addPolygons(
        group = "countries",
        fillColor = ~pal(incidence),
        fillOpacity = 0.8,
        color = "white",
        weight = 0.5,
        
        label = ~paste0(
          name, ": ",
          ifelse(
            is.na(incidence),
            "No data",
            paste0(round(incidence, 2), " per 1,000")
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
}

# Launch the app
shinyApp(ui = ui, server = server)
