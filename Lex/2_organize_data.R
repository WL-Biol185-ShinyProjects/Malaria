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


#year_summary trend line chart!

year_summary <- read_csv("Lex/expansion_csv/year_summary.csv", show_col_types = FALSE)

#list.files("Lex/expansion_csv/year_summary.csv")

trend_long <- year_summary %>%
  select(year, mean_incidence, median_incidence) %>%
  pivot_longer(
    cols      = c(mean_incidence, median_incidence),
    names_to  = "measure",
    values_to = "incidence"
  ) %>%
  mutate(
    measure = recode(
      measure,
      mean_incidence   = "Average across countries",
      median_incidence = "Median country"
    )
  )

trend_plot <- ggplot(trend_long, aes(x = year, y = incidence, color = measure)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Average across countries" = "#c2168c",   # from your map palette
    "Median country"           = "#312c54"
  )) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  labs(
    title    = "Malaria incidence, 2000–2024",
    subtitle = "Cases per 1,000 people at risk, across countries with data",
    x        = NULL,
    y        = "Incidence per 1,000 at risk",
    color    = NULL,
    caption  = "Source: Our World in Data. Averages treat each country equally,\nregardless of population."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )

trend_plot

dir.create("output", showWarnings = FALSE)
ggsave("output/trend_chart.png", trend_plot, width = 9, height = 5.5, dpi = 300)

latest <- year_summary %>% filter(year == max(year))
first  <- year_summary %>% filter(year == min(year))

cat("Countries reporting in", latest$year, ":", latest$n_countries, "\n")
cat("Average incidence:", round(first$mean_incidence, 1), "in", first$year,
    "->", round(latest$mean_incidence, 1), "in", latest$year, "\n")

#MAKING IT INTERACTIVE

library(plotly)
library(htmlwidgets)

year_summary <- read_csv("Lex/expansion_csv/year_summary.csv", show_col_types = FALSE)

trend_long <- year_summary %>%
  select(year, n_countries, mean_incidence, median_incidence) %>%
  pivot_longer(
    cols      = c(mean_incidence, median_incidence),
    names_to  = "measure",
    values_to = "incidence"
  ) %>%
  mutate(
    measure = recode(
      measure,
      mean_incidence   = "Average across countries",
      median_incidence = "Median country"
    ),
    # This is the text that appears when you hover. <br> starts a new line.
    hover_text = paste0(
      "<b>", year, "</b><br>",
      measure, ": ", round(incidence, 1), " per 1,000<br>",
      "Countries reporting: ", n_countries
    )
  )

trend_plot <- ggplot(
  trend_long,
  aes(x = year, y = incidence, color = measure, group = measure, text = hover_text)
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Average across countries" = "#c2168c",
    "Median country"           = "#312c54"
  )) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  labs(
    title = "Malaria incidence, 2000–2024",
    x     = NULL,
    y     = "Incidence per 1,000 at risk",
    color = NULL
  ) +
  theme_minimal(base_size = 14)

interactive_plot <- ggplotly(trend_plot, tooltip = "text") %>%
  layout(
    legend = list(orientation = "h", x = 0, y = -0.15),
    annotations = list(
      x = 0, y = -0.3, xref = "paper", yref = "paper",
      text = "Source: Our World in Data. Averages treat each country equally.",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 11, color = "gray")
    )
  )

interactive_plot
