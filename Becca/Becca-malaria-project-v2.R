# ============================================================
# WHO malaria therapeutic efficacy studies: cleaning, summary, plots
# Requires R >= 4.1
# ============================================================

# ---- 1. Packages -------------------------------------------
packages <- c("tidyverse", "readxl", "janitor")

missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(tidyverse)
library(readxl)
library(janitor)

# ---- 2. Settings -------------------------------------------
# Path to the Excel file. Leave as NULL to pick it in a dialog
# (works only when R is run interactively, e.g. in RStudio).
data_file <- "Becca/MTM_THERAPEUTIC_EFFICACY_STUDY_20261012.xlsx"
data_sheet <- "Data"

# Species to analyse. Mixing species gives misleading drug medians,
# so the default is P. falciparum. Use NULL to keep all species.
analysis_species <- "falciparum"   # matched case-insensitively as text

# Minimum number of records for a drug to appear in the trend plot
min_records_trend <- 5

output_dir <- "Becca"

# ---- 3. Locate and import the file -------------------------
if (is.null(file_path)) {
  if (interactive()) {
    file_path <- file.choose()
  } else {
    stop("Set 'file_path' at the top of the script when running non-interactively.")
  }
}

if (!file.exists(file_path)) {
  stop("Excel file not found: ", file_path)
}

sheets <- excel_sheets(file_path)
print(sheets)

if (!data_sheet %in% sheets) {
  stop("Sheet '", data_sheet, "' not found. Available sheets: ",
       paste(sheets, collapse = ", "))
}

who <- read_excel(file_path, sheet = data_sheet) |>
  clean_names()

cat("\nRows:", nrow(who), " Columns:", ncol(who), "\n")
cat("\nColumn names:\n")
print(names(who))

# ---- 4. Check required columns -----------------------------
required_columns <- c("drug_name", "treatment_failure_pp")
missing_columns  <- setdiff(required_columns, names(who))

if (length(missing_columns) > 0) {
  stop("Missing required columns: ",
       paste(missing_columns, collapse = ", "),
       "\nCompare with the printed column names and update the script.")
}

if (!is.null(analysis_species) && !"plasmodium_species" %in% names(who)) {
  stop("'plasmodium_species' column not found, so the species filter cannot be applied. ",
       "Set analysis_species <- NULL to analyse all records.")
}

# ---- 5. Clean and convert variables ------------------------
# parse_number() handles entries such as "12%" or "1,200".
# We record any value that existed but could not be parsed.
numeric_candidates <- c(
  "year_start", "year_end", "sample_size",
  "latitude", "longitude",
  "treatment_failure_pp", "treatment_failure_km",
  "positive_day_3_days"
)

conversion_log <- list()

for (column in intersect(numeric_candidates, names(who))) {
  original  <- as.character(who[[column]])
  converted <- suppressWarnings(parse_number(original))
  failed    <- sum(!is.na(original) & original != "" & is.na(converted))
  conversion_log[[column]] <- tibble(column = column, unparsable_values = failed)
  who[[column]] <- converted
}

conversion_log <- bind_rows(conversion_log)
cat("\nValues that could not be converted to numbers:\n")
print(conversion_log)

text_candidates <- c("country_name", "admin2", "site_name",
                     "drug_name", "plasmodium_species")

for (column in intersect(text_candidates, names(who))) {
  who[[column]] <- str_squish(as.character(who[[column]]))
}

cat("\nTreatment failure (per-protocol), all records:\n")
print(summary(who$treatment_failure_pp))

# Failure rates stored as proportions (0-1) instead of percentages
# would silently distort every summary, so warn about that.
max_fail <- suppressWarnings(max(who$treatment_failure_pp, na.rm = TRUE))
if (is.finite(max_fail) && max_fail <= 1) {
  warning("All treatment_failure_pp values are <= 1. They may be proportions, not percentages.")
}

# ---- 6. Output folder and data-quality reports -------------
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(who, file.path(output_dir, "who_cleaned.csv"))
write_csv(conversion_log, file.path(output_dir, "conversion_log.csv"))

missing_report <- tibble(
  column          = names(who),
  missing_count   = vapply(who, function(x) sum(is.na(x)), numeric(1)),
  missing_percent = vapply(who, function(x) mean(is.na(x)) * 100, numeric(1))
) |>
  arrange(desc(missing_percent))

print(missing_report, n = Inf)
write_csv(missing_report, file.path(output_dir, "missing_data_report.csv"))

cat("\nDuplicate rows:", sum(duplicated(who)), "\n")

if ("plasmodium_species" %in% names(who)) {
  cat("\nParasite species (all records):\n")
  print(count(who, plasmodium_species, sort = TRUE))
}

cat("\nDrug names (all records):\n")
print(count(who, drug_name, sort = TRUE), n = Inf)

# ---- 7. Restrict to the analysis species -------------------
analysis_data <- who

if (!is.null(analysis_species)) {
  analysis_data <- who |>
    filter(str_detect(str_to_lower(plasmodium_species), 
                      str_to_lower(analysis_species)))
  
  cat("\nRecords for species matching '", analysis_species, "': ",
      nrow(analysis_data), " of ", nrow(who), "\n", sep = "")
  
  if (nrow(analysis_data) == 0) {
    stop("No records match analysis_species = '", analysis_species,
         "'. Check the species counts printed above.")
  }
}

write_csv(analysis_data, file.path(output_dir, "analysis_species_records.csv"))

# ---- 8. Valid treatment-failure records --------------------
failure_data <- analysis_data |>
  filter(
    !is.na(drug_name), drug_name != "",
    !is.na(treatment_failure_pp),
    between(treatment_failure_pp, 0, 100)
  )

excluded <- sum(!is.na(analysis_data$treatment_failure_pp)) - nrow(failure_data)
cat("\nRecords with valid per-protocol failure:", nrow(failure_data), "\n")
cat("Records dropped for being outside 0-100% or lacking a drug name:", excluded, "\n")

if (nrow(failure_data) == 0) stop("No valid treatment failure records to analyse.")

# ---- 9. Summary by drug ------------------------------------
# Median and range treat every study equally. The weighted mean uses
# sample size, so large studies count more. It is NA if sample size is missing.
has_n <- "sample_size" %in% names(failure_data)

drug_summary <- failure_data |>
  group_by(drug_name) |>
  summarise(
    number_of_studies       = n(),
    median_failure_percent  = median(treatment_failure_pp),
    mean_failure_percent    = mean(treatment_failure_pp),
    minimum_failure_percent = min(treatment_failure_pp),
    maximum_failure_percent = max(treatment_failure_pp),
    weighted_mean_failure_percent = if (has_n) {
      ok <- !is.na(sample_size) & sample_size > 0
      if (any(ok)) weighted.mean(treatment_failure_pp[ok], sample_size[ok]) else NA_real_
    } else NA_real_,
    .groups = "drop"
  ) |>
  arrange(desc(median_failure_percent))

print(drug_summary, n = Inf)
write_csv(drug_summary, file.path(output_dir, "treatment_failure_by_drug.csv"))

# ---- 10. Plot: failure by drug -----------------------------
top_drugs <- failure_data |>
  count(drug_name, sort = TRUE) |>
  slice_head(n = 12) |>
  pull(drug_name)

drug_plot_data <- failure_data |>
  filter(drug_name %in% top_drugs) |>
  mutate(drug_name = fct_reorder(drug_name, treatment_failure_pp, .fun = median))

p_drugs <- ggplot(drug_plot_data, aes(x = drug_name, y = treatment_failure_pp)) +
  geom_boxplot(outlier.shape = NA, fill = "skyblue3") +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
  coord_flip() +
  labs(
    title    = "Reported treatment failure by antimalarial drug",
    subtitle = paste0("Top ", length(top_drugs), " drugs by number of studies",
                      if (!is.null(analysis_species)) paste0(" (", analysis_species, ")") else ""),
    x = "Drug",
    y = "Per-protocol treatment failure (%)"
  ) +
  theme_minimal(base_size = 12)

print(p_drugs)
ggsave(file.path(output_dir, "failure_by_drug.png"),
       plot = p_drugs, width = 10, height = 7, dpi = 300)

# ---- 11. Plot: failure over time ---------------------------
if ("year_start" %in% names(failure_data)) {
  
  trend_data <- failure_data |> filter(!is.na(year_start))
  
  if (nrow(trend_data) > 0) {
    
    yearly_data <- trend_data |>
      group_by(drug_name, year_start) |>
      summarise(
        median_failure    = median(treatment_failure_pp),
        number_of_studies = n(),
        .groups = "drop"
      )
    
    write_csv(yearly_data, file.path(output_dir, "yearly_failure_summary.csv"))
    
    eligible_drugs <- trend_data |>
      count(drug_name) |>
      filter(n >= min_records_trend) |>
      pull(drug_name)
    
    yearly_plot_data <- yearly_data |> filter(drug_name %in% eligible_drugs)
    
    if (nrow(yearly_plot_data) > 0) {
      p_trends <- ggplot(
        yearly_plot_data,
        aes(x = year_start, y = median_failure, color = drug_name, group = drug_name)
      ) +
        geom_line(linewidth = 0.8) +
        geom_point(aes(size = number_of_studies)) +
        labs(
          title    = "Treatment failure over time",
          subtitle = "Annual median across available studies (unweighted)",
          x = "Study start year",
          y = "Median treatment failure (%)",
          color = "Drug",
          size  = "Studies in year"
        ) +
        theme_minimal(base_size = 12)
      
      print(p_trends)
      ggsave(file.path(output_dir, "failure_over_time.png"),
             plot = p_trends, width = 11, height = 7, dpi = 300)
    } else {
      message("No drug has at least ", min_records_trend, " records for the trend plot.")
    }
  } else {
    message("No valid study years available; skipping time trend.")
  }
} else {
  message("year_start column not available; skipping time trend.")
}

# ---- 12. Plot: day-3 positivity vs failure -----------------
# Use the expected column if present; otherwise use a single
# unambiguous match; otherwise skip rather than guess.
day3_candidates <- names(who)[
  str_detect(names(who), "positive.*day.?3|day.?3.*positive")
]
cat("\nPossible day-3 columns:", paste(day3_candidates, collapse = ", "), "\n")

day3_col <- if ("positive_day_3_days" %in% names(who)) {
  "positive_day_3_days"
} else if (length(day3_candidates) == 1) {
  day3_candidates
} else {
  NA_character_
}

if (!is.na(day3_col)) {
  
  day3_data <- failure_data |>
    mutate(day3_value = parse_number(as.character(.data[[day3_col]]))) |>
    filter(!is.na(day3_value))
  
  if (nrow(day3_data) > 2) {
    p_day3 <- ggplot(day3_data, aes(x = day3_value, y = treatment_failure_pp)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
      labs(
        title = "Day-3 positivity vs treatment failure",
        subtitle = paste("Column used:", day3_col),
        x = "Day-3 positivity (as recorded)",
        y = "Per-protocol treatment failure (%)"
      ) +
      theme_minimal(base_size = 12)
    
    print(p_day3)
    ggsave(file.path(output_dir, "day3_vs_failure.png"),
           plot = p_day3, width = 9, height = 6, dpi = 300)
  } else {
    message("Not enough complete day-3 records to plot.")
  }
  
} else {
  message("Skipping day-3 plot: no unambiguous day-3 column. ",
          "Set the column name manually using the candidates printed above.")
}

# ---- 13. Finished ------------------------------------------
cat("\n====================================\n")
cat("ANALYSIS COMPLETE\n")
cat("Results saved in:", normalizePath(output_dir), "\n")
cat("====================================\n")
print(list.files(output_dir))