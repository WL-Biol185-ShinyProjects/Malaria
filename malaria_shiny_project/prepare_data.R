# Prepare and validate the malaria evidence datasets for the Shiny app.
# Run from the project root: source("prepare_data.R")
# This script deliberately does NOT invent case values, population denominators,
# insurance coverage, annual expenditure, or historical retrieval dates.

required_packages <- c("readr", "dplyr", "stringr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install packages: ", paste(missing_packages, collapse = ", "))
library(readr); library(dplyr); library(stringr)

case_path <- file.path("data", "who_europe_malaria_indigenous_cases_2000_2015_audited.csv")
event_path <- file.path("data", "azerbaijan_malaria_interventions_financing_2000_2015_audited.csv")
pop_path <- file.path("data", "population_denominators.csv")
insurance_path <- file.path("data", "health_insurance_coverage.csv")

stopifnot(file.exists(case_path), file.exists(event_path))
cases <- read_csv(case_path, show_col_types = FALSE, na = c("", "NA", "NaN"))
events <- read_csv(event_path, show_col_types = FALSE, na = c("", "NA", "NaN"))

# Required columns and stable join keys.
stopifnot(all(c("country", "iso3", "year", "indigenous_cases") %in% names(cases)))
stopifnot(all(c("year_start", "year_end", "iso3_or_iso3_list", "category", "event_or_intervention", "source_url") %in% names(events)))

cases <- cases |>
  mutate(
    iso3 = str_to_upper(str_trim(as.character(iso3))),
    map_key_iso3 = str_to_upper(str_trim(as.character(if ("map_key_iso3" %in% names(cases)) map_key_iso3 else iso3))),
    year = as.integer(year),
    indigenous_cases = suppressWarnings(as.numeric(indigenous_cases))
  )

# Preserve raw/source-reported values. The map value may be blank where a known discrepancy exists.
if (!"source_reported_cases" %in% names(cases)) cases$source_reported_cases <- cases$indigenous_cases
if (!"map_value_cases" %in% names(cases)) cases$map_value_cases <- cases$source_reported_cases
if (!"qa_status" %in% names(cases)) cases$qa_status <- "not_reviewed"
if (!"qa_action" %in% names(cases)) cases$qa_action <- "Review against a dated primary source before publication."
if (!"verification_status" %in% names(cases)) cases$verification_status <- "not_independently_revalidated"
if (!"source_validation_status" %in% names(cases)) cases$source_validation_status <- "source_vintage_not_independently_revalidated"
if (!"source_validation_date" %in% names(cases)) cases$source_validation_date <- NA_character_
if (!"source_validation_note" %in% names(cases)) cases$source_validation_note <- "Review data/source_validation_register.csv; source vintage may be unavailable."

# Normalize known country-name aliases and remove duplicate ISO3-year rows only when values agree.
cases <- cases |>
  mutate(country = case_when(iso3 == "CZE" ~ "Czechia", iso3 == "TUR" ~ "Türkiye", TRUE ~ country))
dup_keys <- cases |> count(map_key_iso3, year) |> filter(n > 1)
if (nrow(dup_keys)) {
  dup_rows <- cases |> semi_join(dup_keys, by = c("map_key_iso3", "year"))
  dup_conflicts <- dup_rows |>
    group_by(map_key_iso3, year) |>
    summarise(
      case_values = n_distinct(source_reported_cases[!is.na(source_reported_cases)]),
      map_values = n_distinct(map_value_cases[!is.na(map_value_cases)]),
      .groups = "drop"
    ) |>
    filter(case_values > 1 | map_values > 1)
  if (nrow(dup_conflicts)) stop("Conflicting duplicate ISO3-year rows found; resolve manually before preparation.")
  cases <- cases |> distinct(map_key_iso3, year, .keep_all = TRUE)
}

# Keep the known Azerbaijan 2012 disagreement visible and do not select a value automatically.
aze_2012 <- cases$iso3 == "AZE" & cases$year == 2012
if (any(aze_2012, na.rm = TRUE)) {
  cases$qa_status[aze_2012] <- "conflicting_sources"
  cases$verification_status[aze_2012] <- "discrepancy_requires_resolution"
  cases$map_value_cases[aze_2012] <- NA_real_
  cases$qa_action[aze_2012] <- paste(
    "Source compilation reports 3; Mammadov et al. (2016) describes 2 indigenous cases among 3 total reported cases.",
    "Resolve against one dated WHO source release before publication; original source-reported value is preserved."
  )
}

# Add provenance, population-rate, and geography fields without making up missing facts.
add_if_missing <- function(data, column, default) {
  if (!column %in% names(data)) data[[column]] <- default
  data
}
cases <- add_if_missing(cases, "source_dataset_version", "Supplied historical compilation; exact WHO release/version not recorded")
cases <- add_if_missing(cases, "source_retrieval_date", NA_character_)
cases <- add_if_missing(cases, "population_denominator", NA_real_)
cases <- add_if_missing(cases, "population_source_citation", NA_character_)
cases <- add_if_missing(cases, "population_source_url", NA_character_)
cases <- add_if_missing(cases, "indigenous_cases_per_100k", NA_real_)
cases <- add_if_missing(cases, "rate_status", "population_denominator_not_joined")
cases <- add_if_missing(cases, "region_scope_note", "WHO European Region as recorded in supplied compilation; verify membership/scope for the selected historical comparison")
cases <- add_if_missing(cases, "geography_join_key", "map_key_iso3")
cases <- add_if_missing(cases, "boundary_source_note", "Natural Earth via rnaturalearth at runtime; record package version and inspect ISO3 mismatches before publication")

# Optional population denominators. Rate is only calculated when both a displayable case count and a positive denominator exist.
if (file.exists(pop_path)) {
  pop <- read_csv(pop_path, show_col_types = FALSE, na = c("", "NA", "NaN"))
  if (all(c("iso3", "year", "population") %in% names(pop)) && nrow(pop) > 0) {
    pop <- pop |>
      mutate(iso3 = str_to_upper(str_trim(as.character(iso3))), year = as.integer(year), population = suppressWarnings(as.numeric(population)))
    dup_pop <- pop |> count(iso3, year) |> filter(n > 1)
    if (nrow(dup_pop)) stop("Population file contains duplicate ISO3-year rows; resolve these before calculating rates.")
    pop_cols <- pop |> select(iso3, year, population, any_of(c("population_source_citation", "population_source_url")))
    cases <- cases |> select(-any_of(c("population", "population_source_citation_joined", "population_source_url_joined"))) |>
      left_join(pop_cols, by = c("iso3", "year"), suffix = c("", "_joined")) |>
      mutate(
        population_denominator = coalesce(as.numeric(population_denominator), as.numeric(population)),
        population_source_citation = coalesce(as.character(population_source_citation), as.character(population_source_citation_joined)),
        population_source_url = coalesce(as.character(population_source_url), as.character(population_source_url_joined)),
        indigenous_cases_per_100k = if_else(!is.na(map_value_cases) & !is.na(population_denominator) & population_denominator > 0,
                                             map_value_cases / population_denominator * 100000, NA_real_),
        rate_status = if_else(!is.na(indigenous_cases_per_100k), "calculated_from_joined_population", "denominator_missing_or_case_value_unresolved")
      ) |>
      select(-any_of(c("population", "population_source_citation_joined", "population_source_url_joined")))
  }
}

# Do not silently resolve duplicates.
duplicates <- cases |> count(map_key_iso3, year) |> filter(n > 1)
negative_counts <- cases |> filter(!is.na(source_reported_cases) & source_reported_cases < 0)
invalid_years <- cases |> filter(is.na(year) | year < 2000 | year > 2015)
if (nrow(duplicates)) stop("Duplicate country-year rows remain after normalization; inspect qa_duplicate_country_year.csv")
if (nrow(negative_counts)) warning("Negative case counts found; see qa_negative_case_counts.csv")
if (nrow(invalid_years)) warning("Rows outside 2000–2015 found; see qa_out_of_period.csv")

# Validate intervention period and explicit finance limitations.
events <- events |>
  mutate(
    year_start = as.integer(year_start), year_end = as.integer(year_end),
    implementation_status = if ("implementation_status" %in% names(events)) implementation_status else "documented_in_source_not_independently_verified",
    annual_amount_usd = if ("annual_amount_usd" %in% names(events)) as.numeric(annual_amount_usd) else NA_real_,
    actual_annual_expenditure_usd = if ("actual_annual_expenditure_usd" %in% names(events)) as.numeric(actual_annual_expenditure_usd) else NA_real_,
    financial_measure_type = if ("financial_measure_type" %in% names(events)) financial_measure_type else if_else(str_to_lower(category) == "financing", "reported_funding_not_verified_annual_expenditure", "not_applicable"),
    insurance_indicator_status = if ("insurance_indicator_status" %in% names(events)) insurance_indicator_status else "no comparable annual insurance coverage series documented"
  )
invalid_event_dates <- events |> filter(!is.na(year_start) & !is.na(year_end) & year_end < year_start)
if (nrow(invalid_event_dates)) warning("Intervention records have end years before start years; review them.")

# Write normalized data back to project data folder.
write_csv(cases, case_path, na = "NA")
write_csv(events, event_path, na = "NA")

# Write QA extracts, including empty extracts with headers where there are no findings.
write_csv(duplicates, file.path("data", "qa_duplicate_country_year.csv"), na = "NA")
write_csv(negative_counts, file.path("data", "qa_negative_case_counts.csv"), na = "NA")
write_csv(invalid_years, file.path("data", "qa_out_of_period.csv"), na = "NA")
write_csv(invalid_event_dates, file.path("data", "qa_invalid_intervention_dates.csv"), na = "NA")
write_csv(cases |> count(qa_status, verification_status, name = "rows"), file.path("data", "qa_status_summary.csv"), na = "NA")

cat("Preparation complete. Review QA reports in data/ before publishing.\n")
cat("Case rows:", nrow(cases), " | Intervention rows:", nrow(events), "\n")
cat("Country-year duplicates:", nrow(duplicates), " | Negative counts:", nrow(negative_counts),
    " | Out-of-period rows:", nrow(invalid_years), " | Invalid intervention dates:", nrow(invalid_event_dates), "\n")
cat("Rates are only calculated when valid population denominators are supplied. Insurance data are separate from malaria interventions.\n")
cat("Missing values are serialized as the literal NA token; read_csv(..., na = c("", "NA", "NaN")) interprets them as missing. Genuine zeroes are preserved.\n")
