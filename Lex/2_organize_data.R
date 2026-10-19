library(tidyverse)

malaria <- read_csv("data/malaria_clean.csv",
                    show_col_types = FALSE)

malaria_sorted <- malaria %>%
  arrange(country, year)

malaria_wide <- malaria %>%
  select(country, code, year, incidence) %>%
  pivot_wider(names_from = year, values_from = incidence) %>%
  arrange(country)

# One row per country: how has incidence changed over time?
country_summary <- malaria %>%
  filter(!is.na(incidence)) %>%
  group_by(country, code) %>%
  summarize(
    first_year      = min(year),
    last_year       = max(year),
    first_incidence = incidence[which.min(year)],
    last_incidence  = incidence[which.max(year)],
    change          = last_incidence - first_incidence,
    mean_incidence  = mean(incidence),
    max_incidence   = max(incidence),
    years_of_data   = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(last_incidence))

# One row per year: the global picture across countries.
year_summary <- malaria %>%
  group_by(year) %>%
  summarize(
    n_countries      = sum(!is.na(incidence)),
    mean_incidence   = mean(incidence, na.rm = TRUE),
    median_incidence = median(incidence, na.rm = TRUE),
    max_incidence    = max(incidence, na.rm = TRUE),
    .groups = "drop"
  )

# Which countries are missing which years?
all_years <- sort(unique(malaria$year))

missing_data <- malaria %>%
  distinct(country, code) %>%
  crossing(year = all_years) %>%                       # every country x every year
  anti_join(malaria %>% filter(!is.na(incidence)),     # remove combos that exist
            by = c("country", "code", "year")) %>%
  arrange(country, year)

head(country_summary, 10)   # highest current incidence
head(year_summary)
nrow(missing_data)          # how many country-year gaps?
# View(malaria_wide)        # opens a spreadsheet-style viewer in RStudio

dir.create("output", showWarnings = FALSE)
write_csv(malaria_wide,     "output/malaria_wide.csv")
write_csv(country_summary,  "output/country_summary.csv")
write_csv(year_summary,     "output/year_summary.csv")
write_csv(missing_data,     "output/missing_data.csv")