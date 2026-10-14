install.packages(c("tidyverse", "sf", "rnaturalearth",
                   "rnaturalearthdata", "leaflet", "viridis"))

library(tidyverse)
library(sf)
library(rnaturalearth)
library(leaflet)
library(viridis)

# Download the same indicator used by Our World in Data
malaria <- read_csv(
  "incidence-of-malaria.csv"
)

# Inspect the data
names(malaria)
head(malaria)

# Get world country boundaries
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Inspect the data
names(malaria)
names(world)
head(malaria)

glimpse(malaria)
glimpse(world)

# Select the year to display
map_year <- 2006

# Filter malaria data to that year
malaria_2006 <- malaria %>%
  filter(Year == map_year, !is.na(Code)) %>%
  select(Code, incidence = `Incidence of malaria (per 1,000 population at risk)`)

# Join the incidence data to country boundaries
world_malaria <- world %>%
  left_join(malaria_2006, by = c("iso_a3" = "Code"))

my_colors <- c(
  "#f187d0",
  "#c2168c",
  "#890f63",
  "#312c54",
  "#0c0434"
)

# Draw the interactive map
pal <- colorNumeric(
  palette = my_colors,
  domain = world_malaria$incidence,
  na.color = "#d9d9d9"
)

leaflet(world_malaria) %>%
  addTiles() %>%
  addPolygons(
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
    pal = pal,
    values = ~incidence,
    title = "Malaria incidence<br/>per 1,000 at risk",
    position = "bottomright"
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


# Apply the palette to your malaria map
pal <- colorNumeric(
  palette = my_colors,
  domain = color_domain,
  na.color = "#d9d9d9"
) 
