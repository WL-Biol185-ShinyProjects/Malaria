# 1. INSTALL AND LOAD PACKAGES --------------------------------

packages <- c("tidyverse",
              "readxl", 
              "janitor", "readr")

missing_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)
library(readxl)
library(janitor)
library(readr)


# 2. SELECT YOUR EXCEL FILE -----------------------------------

file_path <- file.choose()

# If R cannot find the file, uncomment the next line
# and select the Excel file manually:
# file_path <- file.choose()

if (!file.exists(file_path)) {
  stop(
    "Excel file not found. Check file_path or use file.choose()."
  )
}

# Check available worksheets
print(excel_sheets(file_path))

# Import the Data worksheet
who_raw <- read_excel(file_path, sheet = "Data")

# Standardize column names
who <- who_raw |>
  clean_names()

cat("\nNumber of rows:", nrow(who), "\n")
cat("Number of columns:", ncol(who), "\n")

cat("\nColumn names:\n")
print(names(who))


# 3. CHECK REQUIRED COLUMNS -----------------------------------

required_columns <- c(
  "drug_name",
  "treatment_failure_pp"
)

missing_columns <- setdiff(required_columns, names(who))

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", "),
    "\nReview the printed column names and update the script."
  )
}


# 4. CLEAN AND CONVERT VARIABLES ------------------------------

# Convert numeric-looking columns safely.
# parse_number() handles values such as "12%" or "1,200".

numeric_candidates <- c(
  "year_start",
  "year_end",
  "sample_size",
  "latitude",
  "longitude",
  "treatment_failure_pp",
  "treatment_failure_km",
  "positive_day_3_days"
)

for (column in intersect(numeric_candidates, names(who))) {
  who[[column]] <- parse_number(
    as.character(who[[column]])
  )
}

# Standardize text columns without changing original names
text_candidates <- c(
  "country_name",
  "admin2",
  "site_name",
  "drug_name",
  "plasmodium_species"
)

for (column in intersect(text_candidates, names(who))) {
  who[[column]] <- stringr::str_squish(
    as.character(who[[column]])
  )
}

# Inspect treatment failure values before analysis
cat("\nTreatment failure summary:\n")
print(summary(who$treatment_failure_pp))

cat("\nMissing treatment failure values:\n")
print(sum(is.na(who$treatment_failure_pp)))


# 5. CREATE OUTPUT FOLDER -------------------------------------

output_dir <- "malaria_analysis_results"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# 6. SAVE CLEANED DATA AND DATA-QUALITY REPORT ----------------

write_csv(
  who,
  file.path(output_dir, "who_cleaned.csv")
)

# Missingness for every column
missing_report <- tibble(
  column = names(who),
  missing_count = vapply(
    who,
    function(x) sum(is.na(x)),
    numeric(1)
  ),
  missing_percent = vapply(
    who,
    function(x) mean(is.na(x)) * 100,
    numeric(1)
  )
) |>
  arrange(desc(missing_percent))

print(missing_report)

write_csv(
  missing_report,
  file.path(output_dir, "missing_data_report.csv")
)

# Basic data checks
cat("\nDuplicate rows:", sum(duplicated(who)), "\n")

if ("plasmodium_species" %in% names(who)) {
  cat("\nParasite species:\n")
  print(count(who, plasmodium_species, sort = TRUE))
}

cat("\nDrug names:\n")
print(count(who, drug_name, sort = TRUE))


# 7. PREPARE TREATMENT FAILURE DATA ----------------------------

# Keep valid percentages.
# Treatment failure is a percentage, not a count.

failure_data <- who |>
  filter(
    !is.na(drug_name),
    drug_name != "",
    !is.na(treatment_failure_pp),
    between(treatment_failure_pp, 0, 100)
  )

cat(
  "\nStudies with valid per-protocol failure:",
  nrow(failure_data),
  "\n"
)


# 8. SUMMARIZE TREATMENT FAILURE BY DRUG -----------------------

drug_summary <- failure_data |>
  group_by(drug_name) |>
  summarise(
    number_of_studies = n(),
    median_failure_percent = median(
      treatment_failure_pp,
      na.rm = TRUE
    ),
    mean_failure_percent = mean(
      treatment_failure_pp,
      na.rm = TRUE
    ),
    minimum_failure_percent = min(
      treatment_failure_pp,
      na.rm = TRUE
    ),
    maximum_failure_percent = max(
      treatment_failure_pp,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(desc(median_failure_percent))

print(drug_summary, n = Inf)

write_csv(
  drug_summary,
  file.path(output_dir, "treatment_failure_by_drug.csv")
)


# 9. GRAPH: TREATMENT FAILURE BY DRUG --------------------------

# Show the most frequently studied drugs to keep the graph
# readable. The summary table above retains all drugs.

top_drugs <- failure_data |>
  count(drug_name, sort = TRUE) |>
  slice_head(n = 12) |>
  pull(drug_name)

drug_plot_data <- failure_data |>
  filter(drug_name %in% top_drugs) |>
  mutate(
    drug_name = forcats::fct_reorder(
      drug_name,
      treatment_failure_pp,
      .fun = median,
      .desc = FALSE
    )
  )

p_drugs <- ggplot(
  drug_plot_data,
  aes(x = drug_name, y = treatment_failure_pp)
) +
  geom_boxplot(outlier.shape = NA, fill = "skyblue3") +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
  coord_flip() +
  labs(
    title = "Reported treatment failure by antimalarial drug",
    subtitle = "Top 12 drugs by number of studies",
    x = "Drug",
    y = "Per-protocol treatment failure (%)"
  ) +
  theme_minimal(base_size = 12)

print(p_drugs)

ggsave(
  file.path(output_dir, "failure_by_drug.png"),
  plot = p_drugs,
  width = 10,
  height = 7,
  dpi = 300
)


# 10. GRAPH: TREATMENT FAILURE OVER TIME -----------------------

if ("year_start" %in% names(who)) {
  
  trend_data <- failure_data |>
    filter(!is.na(year_start))
  
  if (nrow(trend_data) > 0) {
    
    # Plot individual studies and yearly median values.
    # Yearly medians are descriptive, not patient-weighted.
    
    yearly_data <- trend_data |>
      group_by(drug_name, year_start) |>
      summarise(
        median_failure = median(
          treatment_failure_pp,
          na.rm = TRUE
        ),
        number_of_studies = n(),
        .groups = "drop"
      )
    
    write_csv(
      yearly_data,
      file.path(output_dir, "yearly_failure_summary.csv")
    )
    
    # Use drugs with at least 5 observations overall
    eligible_drugs <- trend_data |>
      count(drug_name) |>
      filter(n >= 5) |>
      pull(drug_name)
    
    yearly_plot_data <- yearly_data |>
      filter(drug_name %in% eligible_drugs)
    
    if (nrow(yearly_plot_data) > 0) {
      
      p_trends <- ggplot(
        yearly_plot_data,
        aes(
          x = year_start,
          y = median_failure,
          color = drug_name,
          group = drug_name
        )
      ) +
        geom_line(linewidth = 0.8) +
        geom_point(aes(size = number_of_studies)) +
        labs(
          title = "Treatment failure over time",
          subtitle = "Annual median across available studies",
          x = "Study start year",
          y = "Median treatment failure (%)",
          color = "Drug",
          size = "Number of studies"
        ) +
        theme_minimal(base_size = 12)
      
      print(p_trends)
      
      ggsave(
        file.path(output_dir, "failure_over_time.png"),
        plot = p_trends,
        width = 11,
        height = 7,
        dpi = 300
      )
      
    } else {
      message(
        "No drugs have at least 5 observations for the trend plot."
      )
    }
    
  } else {
    message("No valid study years are available.")
  }
  
} else {
  message("year_start column not available; skipping time analysis.")
}


# 11. OPTIONAL: DAY-3 POSITIVITY VS TREATMENT FAILURE ---------

# Locate the day-3 column, since its name may vary by export.
day3_candidates <- names(who)[
  stringr::str_detect(
    names(who),
    "positive.*day.?3|day.?3.*positive"
  )
]

cat("\nPossible day-3 columns:\n")
print(day3_candidates)

# Use the expected column name if it exists.
# Otherwise, skip this analysis rather than guessing.

if ("positive_day_3_days" %in% names(who)) {
  
  day3_data <- who |>
    mutate(
      day3_value = parse_number(
        as.character(positive_day_3_days)
      )
    ) |>
    filter(
      !is.na(day3_value),
      !is.na(treatment_failure_pp),
      between(treatment_failure_pp, 0, 100)
    )
  
  if (nrow(day3_data) > 2) {
    
    p_day3 <- ggplot(
      day3_data,
      aes(
        x = day3_value,
        y = treatment_failure_pp
      )
    ) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(
        title = "Day-3 positivity measure vs treatment failure",
        x = "Recorded day-3 measure",
        y = "Per-protocol treatment failure (%)"
      ) +
      theme_minimal(base_size = 12)
    
    print(p_day3)
    
    ggsave(
      file.path(output_dir, "day3_vs_failure.png"),
      plot = p_day3,
      width = 9,
      height = 6,
      dpi = 300
    )
    
  } else {
    message("Not enough complete day-3 records to plot.")
  }
  
} else {
  message(
    "Skipping day-3 comparison. Check the printed candidate ",
    "column names and the data glossary before adding it."
  )
}


# 12. OPTIONAL: FOCUS ON PLASMODIUM FALCIPARUM ---------------

if ("plasmodium_species" %in% names(who)) {
  
  falciparum_data <- failure_data |>
    filter(
      stringr::str_detect(
        stringr::str_to_lower(plasmodium_species),
        "falciparum"
      )
    )
  
  write_csv(
    falciparum_data,
    file.path(output_dir, "falciparum_studies.csv")
  )
  
  cat(
    "\nValid P. falciparum study records:",
    nrow(falciparum_data),
    "\n"
  )
}


# 13. FINISHED ------------------------------------------------

cat("\n====================================\n")
cat("ANALYSIS COMPLETE\n")
cat("Results saved in:", output_dir, "\n")
cat("====================================\n")

# List files created
print(list.files(output_dir))

