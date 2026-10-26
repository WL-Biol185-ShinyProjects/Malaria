# Malaria elimination website data preparation
# Run from the folder containing the two input CSVs.
# Keeps source-reported values separate from values safe to display on the map.

install.packages(c(
  "shiny", "leaflet", "dplyr", "readr",
  "rnaturalearth", "rnaturalearthdata"
))

library(readr)
library(dplyr)
library(stringr)

cases <- read_csv("who_europe_malaria_indigenous_cases_2000_2015.csv", show_col_types = FALSE,
                  na = c("", "NA"))
interventions <- read_csv("malaria_interventions_financing_azerbaijan_comparative_2000_2015.csv",
                          show_col_types = FALSE, na = c("", "NA"))

# Basic structure checks
stopifnot(all(c("country", "iso3", "year", "indigenous_cases") %in% names(cases)))
stopifnot(!anyDuplicated(cases[c("iso3", "year")]))
stopifnot(all(cases$year %in% 2000:2015))

cases_audited <- cases |>
  mutate(
    map_key_iso3 = str_to_upper(str_trim(iso3)),
    source_reported_cases = indigenous_cases,
    map_value_cases = indigenous_cases,
    qa_status = case_when(
      is.na(indigenous_cases) ~ "missing_not_zero",
      iso3 == "AZE" & year == 2012 ~ "conflicting_sources",
      indigenous_cases == 0 ~ "reported_zero_unvalidated",
      TRUE ~ "needs_source_vintage_validation"
    ),
    qa_action = case_when(
      is.na(indigenous_cases) ~ "Leave missing; do not recode as zero.",
      iso3 == "AZE" & year == 2012 ~ paste(
        "Source compilation reports 3; 2016 country study describes 2 indigenous",
        "among 3 total. Resolve against the chosen WHO vintage before publication."
      ),
      indigenous_cases == 0 ~ "Retain as reported zero; validate against chosen WHO vintage.",
      TRUE ~ "Confirm against one dated WHO data release before inferential analysis."
    ),
    verification_status = case_when(
      qa_status == "conflicting_sources" ~ "discrepancy_requires_resolution",
      qa_status == "missing_not_zero" ~ "missing_in_compiled_source",
      qa_status == "reported_zero_unvalidated" ~ "reported_zero_not_independently_revalidated",
      TRUE ~ "compiled_value_not_independently_revalidated"
    ),
    # Hide only the disputed AZE 2012 value in the default map layer; preserve the raw value.
    map_value_cases = if_else(iso3 == "AZE" & year == 2012,
                              NA_real_, as.numeric(map_value_cases))
  )

write_csv(cases_audited, "who_europe_malaria_indigenous_cases_2000_2015_audited.csv", na = "")

interventions_audited <- interventions |>
  mutate(
    financial_measure_type = if_else(
      str_to_lower(category) == "financing",
      "reported_contribution_or_support_not_annual_expenditure",
      "not_applicable"
    ),
    annual_amount_usd = NA_real_,
    coverage_percent = NA_real_,
    insurance_coverage_measure = "not_reported_in_source",
    map_display_recommendation = case_when(
      str_to_lower(category) == "financing" ~ "timeline_and_financing_panel",
      str_to_lower(category) == "cross_border_cooperation" ~ "bilateral_timeline_do_not_treat_as_Azerbaijan_only",
      str_detect(str_to_lower(event_or_intervention), "last indigenous") ~ "timeline_with_discrepancy_warning",
      TRUE ~ "timeline_only"
    )
  )
write_csv(interventions_audited, "azerbaijan_malaria_interventions_financing_2000_2015_audited.csv", na = "")

# Quick reports to review before publishing
print(cases_audited |> count(qa_status))
print(cases_audited |> filter(iso3 == "AZE") |> select(year, indigenous_cases,
                                                       map_value_cases, qa_status, qa_action) |> arrange(year))
print(interventions_audited |> select(year_start, year_end, category,
                                      event_or_intervention, financial_measure_type, map_display_recommendation))

# Suggested mapping rule:
# Use map_value_cases for map fill. NA should render as "data unresolved/unavailable",
# not zero. Use indigenous_cases/source_reported_cases in a detail table with QA notes.
# Never interpret intervention timing alone as evidence of causation.
