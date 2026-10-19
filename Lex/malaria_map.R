#How is this working. There was a problems before and not it just magically works. 

library(shiny) #the app framework
library(tidyverse) #read_csv()
library(sf) #spatial data --> for the map
library(rnaturalearth) #country boundries
library(leaflet) #interactive map

# Import the Our World in Data dataset
malaria <- read_csv(
  "https://ourworldindata.org/grapher/incidence-of-malaria.csv?v=1&csvType=full&useColumnShortNames=false",
  show_col_types = FALSE)

# Identify the relevant columns in the downloaded data
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

# Stop with a useful message if a column is missing
if (any(is.na(c(entity_col, code_col, year_col, value_col)))) {
  stop("A required column was not found. Check names(malaria).")
}

# Standardize column names for easier coding
malaria_clean <- malaria %>%
  transmute(
    country = .data[[entity_col]],
    code = .data[[code_col]],
    year = as.integer(.data[[year_col]]),
    incidence = as.numeric(.data[[value_col]])
  )

# Obtain world country boundaries
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Use standardized country codes where available
boundary_code <- if ("iso_a3_eh" %in% names(world)) {
  "iso_a3_eh"
} else {
  "iso_a3"
}

# Establish one consistent color scale across all years
color_domain <- range(malaria_clean$incidence, na.rm = TRUE)

# Your custom purple and pink palette
my_colors <- c(
  "#f187d0",
  "#c2168c",
  "#890f63",
  "#312c54",
  "#0c0434"
)

# Apply the palette to your malaria map
pal <- colorNumeric(
  palette = my_colors,
  domain = color_domain,
  na.color = "#d9d9d9"
) 


# Build the Shiny interface
ui <- fluidPage(
  titlePanel("Global Malaria Incidence, 2001–2024"),
  
  p("Incidence per 1,000 people at risk"),
  
  sliderInput(
    inputId = "year",
    label = "Select year:",
    min = 2001,
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

# Update the map when the selected year changes
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(world) %>%
      addTiles() %>%
      setView(lng = 15, lat = 15, zoom = 2)
  })
  
  observeEvent(input$year, {
    
    # Get data for the selected year
    year_data <- malaria_clean %>%
      filter(year == input$year, !is.na(code)) %>%
      select(code, incidence)
    
    # Join the selected year's data to the map
    world_year <- world %>%
      left_join(
        year_data,
        by = setNames("code", boundary_code)
      )
    
    # Update the map without recreating its base layer
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
            round(incidence, 2)
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
        title = paste(
          "Incidence per 1,000 at risk —",
          input$year
        ),
        layerId = "legend"
      )
    
  }, ignoreInit = FALSE)
}

# Launch the interactive application
shinyApp(ui = ui, server = server)



