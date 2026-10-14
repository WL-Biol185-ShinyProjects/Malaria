
# Install these packages once if needed
install.packages(c("tidyverse", "readxl", "janitor"))

library(tidyverse)
library(readxl)
library(janitor)

# Import the WHO Excel file
who <- read_excel(
  "MTM_THERAPEUTIC_EFFICACY_STUDY_20261012.xlsx",
  sheet = "Data"
) |>
  clean_names()

# Inspect the dataset
dim(who)
names(who)
glimpse(who)
summary(who)

# Which parasite species are represented?
who |>
  count(plasmodium_species, sort = TRUE)

# Which drugs are most common?
who |>
  count(drug_name, sort = TRUE) |>
  print(n = 30)

# Examine study coverage by year
who |>
  count(year_start, sort = FALSE) |>
  ggplot(aes(x = year_start, y = n)) +
  geom_col() +
  labs(
    title = "WHO therapeutic efficacy studies by year",
    x = "Study start year",
    y = "Number of records"
  ) +
  theme_minimal()

# Filter to P. falciparum studies
pf <- who |>
  filter(plasmodium_species == "P. falciparum")

# Summarize treatment outcomes by drug
pf |>
  group_by(drug_name) |>
  summarise(
    studies = n(),
    median_failure_pp = median(
      treatment_failure_pp, na.rm = TRUE
    ),
    median_failure_km = median(
      treatment_failure_km, na.rm = TRUE
    ),
    median_day3_positive = median(
      positive_day_3_days, na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(desc(median_failure_pp))

#Making diagrams with only the WHO data

install.packages(c(
  "tidyverse", "readxl", "janitor",
  "sf", "rnaturalearth"
))


library(tidyverse)
library(sf)
library(rnaturalearth)

# Count studies at each coordinate pair
site_counts <- map_data |>
  count(longitude, latitude, name = "number_of_studies")

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

ggplot() +
  geom_sf(
    data = world,
    fill = "grey95",
    color = "grey75",
    linewidth = 0.2
  ) +
  geom_point(
    data = site_counts,
    aes(
      x = longitude,
      y = latitude,
      size = number_of_studies
    ),
    color = "firebrick",
    alpha = 0.55
  ) +
  scale_size_area(
    max_size = 7,
    name = "Studies at\ncoordinate"
  ) +
  coord_sf(
    default_crs = st_crs(4326),
    lims_method = "geometry_bbox"
  ) +
  labs(
    title = "Global distribution of malaria efficacy studies",
    subtitle = "Circle size represents studies at each coordinate",
    x = NULL,
    y = NULL,
    caption = "Source: WHO therapeutic efficacy study database"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10),
    legend.position = "right",
    plot.title.position = "plot",
    plot.margin = margin(15, 20, 15, 15)
  )



install.packages(c("leaflet", "htmltools"))
library(tidyverse)
library(sf)
library(rnaturalearth)
library(readr)

# Prepare data and ensure columns are numeric
failure_map <- who |>
  mutate(
    latitude = parse_number(as.character(latitude)),
    longitude = parse_number(as.character(longitude)),
    treatment_failure_pp = parse_number(
      as.character(treatment_failure_pp)
    )
  ) |>
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(treatment_failure_pp),
    between(latitude, -90, 90),
    between(longitude, -180, 180),
    between(treatment_failure_pp, 0, 100)
  )

# Load world map
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Plot treatment failure
map2 <- ggplot() +
  geom_sf(
    data = world,
    fill = "grey95",
    color = "grey75",
    linewidth = 0.2
  ) +
  geom_point(
    data = failure_map,
    aes(
      x = longitude,
      y = latitude,
      color = treatment_failure_pp
    ),
    size = 2,
    alpha = 0.65
  ) +
  
  scale_color_viridis_c(
    option = "C",
    limits = c(0, 50),
    breaks = c(0, 10, 20, 30, 40, 50),
    oob = scales::squish,
    name = "Treatment failure (%)"

  ) +
  coord_sf(
    default_crs = st_crs(4326),
    lims_method = "geometry_bbox"
  ) +
  labs(
    title = "Global distribution of antimalarial treatment failure",
    subtitle = "WHO therapeutic efficacy studies",
    caption = paste(
      "Source: WHO therapeutic efficacy study database.",
      "Each point represents a study record."
    ),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(size = 10),
    legend.position = "right",
    plot.title.position = "plot",
    plot.margin = margin(15, 20, 15, 15)
  )

print(map2)

# Save a high-resolution copy
ggsave(
  "malaria_treatment_failure_map.png",
  plot = map2,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

library(tidyverse)
library(leaflet)
library(htmltools)
library(readr)

# Prepare the data
failure_map <- who |>
  mutate(
    latitude = parse_number(as.character(latitude)),
    longitude = parse_number(as.character(longitude)),
    treatment_failure_pp = parse_number(
      as.character(treatment_failure_pp)
    )
  ) |>
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(treatment_failure_pp),
    between(latitude, -90, 90),
    between(longitude, -180, 180),
    between(treatment_failure_pp, 0, 100)
  )

# Color scale from 0% to 50%
pal <- colorNumeric(
  palette = "viridis",
  domain = c(0, 50),
  na.color = "#808080"
)

# Create clickable map

map2_interactive <- leaflet(failure_map) |>
  addTiles()|>
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,
    color = ~pal(treatment_failure_pp),
    fillOpacity = 0.75,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0(
      "<b>Country:</b> ",
      htmlEscape(as.character(country_name)),
      "<br><b>Study site:</b> ",
      htmlEscape(as.character(site_name)),
      "<br><b>Year:</b> ",
      year_start,
      "<br><b>Drug:</b> ",
      htmlEscape(as.character(drug_name)),
      "<br><b>Species:</b> ",
      htmlEscape(as.character(plasmodium_species)),
      "<br><b>Treatment failure:</b> ",
      treatment_failure_pp, "%"
    )
  ) |>
  addLegend(
    position = "bottomright",
    pal = pal,
    values = c(0, 50),
    title = "Treatment failure (%)",
    labFormat = labelFormat(
      suffix = "%",
      between = "–"
    )
  )

map2_interactive
 
