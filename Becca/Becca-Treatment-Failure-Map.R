# ============================================================
# WHO malaria therapeutic efficacy studies: analysis and maps
# Requires R >= 4.1 (native pipe) and ggplot2 >= 3.5.0
# ============================================================

# ---- 1. Packages -------------------------------------------
pkgs <- c("tidyverse", "readxl", "janitor", "sf",
          "rnaturalearth", "rnaturalearthdata",
          "leaflet", "htmltools", "htmlwidgets")

# Installs only what is missing
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(rnaturalearth)
library(leaflet)
library(htmltools)
library(htmlwidgets)

# ---- 2. Settings -------------------------------------------
data_file <- "Becca/MTM_THERAPEUTIC_EFFICACY_STUDY_20261012.xlsx"
data_sheet <- "Data"

# Set to a species name to restrict the maps, or NULL to map everything
map_species <- "P. falciparum"

# Upper limit of the colour scale (values above are shown in the top colour)
fail_cap <- 50

# ---- 3. Import ---------------------------------------------
who <- read_excel(data_file, sheet = data_sheet) |>
  clean_names()

# Make sure numeric columns are numeric (they often import as text)
numeric_cols <- c("latitude", "longitude", "year_start",
                  "treatment_failure_pp", "treatment_failure_km",
                  "positive_day_3_days")

who <- who |>
  mutate(across(any_of(numeric_cols),
                ~ parse_number(as.character(.x))))

# ---- 4. Inspect --------------------------------------------
dim(who)
names(who)
glimpse(who)
summary(who)

# Species represented
who |>
  count(plasmodium_species, sort = TRUE)

# Most common drugs
who |>
  count(drug_name, sort = TRUE) |>
  print(n = 30)

# Study coverage by year
who |>
  filter(!is.na(year_start)) |>
  count(year_start) |>
  ggplot(aes(x = year_start, y = n)) +
  geom_col() +
  labs(
    title = "WHO therapeutic efficacy studies by year",
    x = "Study start year",
    y = "Number of records"
  ) +
  theme_minimal()

# ---- 5. P. falciparum summary by drug ----------------------
pf <- who |>
  filter(plasmodium_species == "P. falciparum")

pf_summary <- pf |>
  group_by(drug_name) |>
  summarise(
    studies              = n(),
    median_failure_pp    = median(treatment_failure_pp, na.rm = TRUE),
    median_failure_km    = median(treatment_failure_km, na.rm = TRUE),
    median_day3_positive = median(positive_day_3_days, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(median_failure_pp))

print(pf_summary, n = Inf)

# ---- 6. Prepare mapping data -------------------------------
map_source <- if (is.null(map_species)) {
  who
} else {
  who |> filter(plasmodium_species == map_species)
}

# Valid coordinates only
geo_data <- map_source |>
  filter(
    !is.na(latitude), !is.na(longitude),
    between(latitude, -90, 90),
    between(longitude, -180, 180)
  )

# Records that also have a plausible failure rate
failure_map <- geo_data |>
  filter(
    !is.na(treatment_failure_pp),
    between(treatment_failure_pp, 0, 100)
  )

# Shared theme and base map
world <- ne_countries(scale = "medium", returnclass = "sf")

map_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10),
    legend.position = "right",
    plot.title.position = "plot",
    plot.margin = margin(15, 20, 15, 15)
  )

# ---- 7. Map 1: number of studies per coordinate ------------
site_counts <- geo_data |>
  count(longitude, latitude, name = "number_of_studies")

map1 <- ggplot() +
  geom_sf(data = world, fill = "grey95",
          color = "grey75", linewidth = 0.2) +
  geom_point(
    data = site_counts,
    aes(x = longitude, y = latitude, size = number_of_studies),
    color = "firebrick", alpha = 0.55
  ) +
  scale_size_area(max_size = 7, name = "Studies at\ncoordinate") +
  coord_sf(default_crs = st_crs(4326), lims_method = "geometry_bbox") +
  labs(
    title = "Global distribution of malaria efficacy studies",
    subtitle = "Circle size represents studies at each coordinate",
    x = NULL, y = NULL,
    caption = "Source: WHO therapeutic efficacy study database"
  ) +
  map_theme

print(map1)

ggsave("malaria_study_locations_map.png", plot = map1,
       width = 12, height = 7, units = "in", dpi = 300)

# ---- 8. Map 2: treatment failure (static) ------------------
map2 <- ggplot() +
  geom_sf(data = world, fill = "grey95",
          color = "grey75", linewidth = 0.2) +
  geom_point(
    data = failure_map,
    aes(x = longitude, y = latitude, color = treatment_failure_pp),
    size = 2, alpha = 0.65
  ) +
  scale_color_viridis_c(
    option = "C",
    limits = c(0, fail_cap),
    breaks = seq(0, fail_cap, by = 10),
    oob = scales::squish,
    name = "Treatment failure (%)"
  ) +
  coord_sf(default_crs = st_crs(4326), lims_method = "geometry_bbox") +
  labs(
    title = "Global distribution of antimalarial treatment failure",
    subtitle = "WHO therapeutic efficacy studies",
    caption = paste("Source: WHO therapeutic efficacy study database.",
                    "Each point represents a study record."),
    x = NULL, y = NULL
  ) +
  map_theme

print(map2)

ggsave("malaria_treatment_failure_map.png", plot = map2,
       width = 12, height = 7, units = "in", dpi = 300)

# ---- 9. Map 3: treatment failure (interactive) -------------
pal <- colorNumeric(
  palette = "viridis",
  domain = c(0, fail_cap),
  na.color = "#808080"
)

# Cap values so anything above fail_cap gets the top colour
# (matches the static map instead of turning grey)
failure_map <- failure_map |>
  mutate(failure_capped = pmin(treatment_failure_pp, fail_cap))

map2_interactive <- leaflet(failure_map) |>
  addTiles() |>
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,
    color = ~pal(failure_capped),
    fillOpacity = 0.75,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0(
      "<b>Country:</b> ",   htmlEscape(as.character(country_name)),
      "<br><b>Study site:</b> ", htmlEscape(as.character(site_name)),
      "<br><b>Year:</b> ",  htmlEscape(as.character(year_start)),
      "<br><b>Drug:</b> ",  htmlEscape(as.character(drug_name)),
      "<br><b>Species:</b> ", htmlEscape(as.character(plasmodium_species)),
      "<br><b>Treatment failure:</b> ",
      round(treatment_failure_pp, 1), "%"
    )
  ) |>
  addLegend(
    position = "bottomright",
    pal = pal,
    values = c(0, fail_cap),
    title = "Treatment failure (%)",
    labFormat = labelFormat(suffix = "%")
  )

map2_interactive

# Save as a standalone HTML file you can share
saveWidget(map2_interactive, "malaria_treatment_failure_map.html",
           selfcontained = TRUE)
