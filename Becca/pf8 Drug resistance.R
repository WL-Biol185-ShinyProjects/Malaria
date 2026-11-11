# =====================================================================
# MalariaGEN Pf8 drug-resistance markers
#   Part A: marker trends over time (country / region)
#   Part B: admin-1 maps of marker prevalence
#   Part C: comparison of Pf8 populations (AF-W, AS-SE-E, ...)
#   Part D: spread of kelch13 mutations by year and country
#   Part E: link to WHO therapeutic efficacy studies (TES)
#
# Requires: R >= 4.1, dplyr >= 1.1.0, ggplot2 >= 3.5.0
#
# Input files
#   Pf8 samples file                 (one row per sample)
#   Pf8 drug-resistance genotypes    (one row per QC-passed sample)
#   WHO TES Excel file               (optional; Part E is skipped if absent)
# =====================================================================


# ---- 1. Packages -----------------------------------------------------
packages <- c("tidyverse", "janitor", "readxl", "sf",
              "rnaturalearth", "rnaturalearthdata", "scales")

missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(tidyverse)
library(janitor)
library(readxl)
library(sf)
library(rnaturalearth)
library(scales)


# ---- 2. Settings -----------------------------------------------------
pf8_samples_file  <- "Pf8_samples__1_.txt"
pf8_genotype_file <- "Pf8_drug_resistance_marker_genotypes.tsv"

who_file  <- "Becca/MTM_THERAPEUTIC_EFFICACY_STUDY_20261012.xlsx"
who_sheet <- "Data"
run_who_analysis <- TRUE

output_dir <- "pf8_results"

# A sample with both a mutant and a wild-type allele (mixed infection)
# counts as carrying the mutation when TRUE.
mixed_counts_as_mutant <- TRUE

# In the Pf8 kelch13 column a blank means "no non-synonymous change"
# (wild type) and "-" means "not called". This was checked against the
# data: blanks are rare in Cambodia/Vietnam and ~100% in most African
# countries. Set FALSE if your file uses blanks for missing instead.
k13_blank_means_wildtype <- TRUE

# Minimum samples before a prevalence estimate is shown
min_n_trend <- 10    # per group and year, Parts A and D
min_n_map   <- 10    # per admin-1 region / population, Parts B and C
min_n_link  <- 20    # Pf8 samples matched to a WHO study, Part E

# Pf8 samples used for Parts B, C: c(first_year, last_year); use -Inf/Inf for all.
# Pooling all years mixes old and recent parasite populations.
summary_year_range <- c(-Inf, Inf)

# WHO study is matched to Pf8 samples from the same country within
# +/- this many years of the study mid-year
year_window <- 2

# Regions are defined by each country's dominant Pf8 population code
sea_populations         <- c("AS-SE-E", "AS-SE-W")
east_africa_populations <- c("AF-E")

# WHO-listed kelch13 mutations (check against the current WHO list)
k13_validated <- c("F446I", "N458Y", "C469Y", "M476I", "Y493H", "R539T",
                   "I543T", "P553L", "R561H", "P574L", "C580Y", "R622I",
                   "A675V")
k13_candidate <- c("P441L", "G449A", "C469F", "A481V", "R515K", "P527T",
                   "N537I", "N537D", "G538V", "V568G")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


# ---- 3. Helper functions --------------------------------------------
resolve_path <- function(path, label) {
  if (!is.null(path) && file.exists(path)) return(path)
  if (interactive()) {
    message("File not found (", path, "). Select the ", label, ".")
    return(file.choose())
  }
  stop(label, " not found: ", path)
}

# Classify one marker column into wildtype / mixed / mutant / NA (missing).
#   wild_type : allele regarded as wild type
#   mutant    : "|"-separated mutant alleles; NA = any non-wild-type allele
classify_marker <- function(x, wild_type, mutant = NA_character_) {
  x <- as.character(x)
  is_missing <- is.na(x) | trimws(x) %in% c("", "-")
  tokens  <- strsplit(toupper(gsub("\\*", "", x)), "[,/]")
  mut_set <- if (is.na(mutant)) NULL else toupper(strsplit(mutant, "\\|")[[1]])
  
  has_mut <- vapply(tokens, function(t) {
    t <- trimws(t)
    t <- t[!is.na(t) & t != ""]
    if (is.null(mut_set)) any(t != wild_type) else any(t %in% mut_set)
  }, logical(1))
  
  has_wt <- vapply(tokens, function(t) wild_type %in% trimws(t), logical(1))
  
  case_when(
    is_missing        ~ NA_character_,
    has_mut & has_wt  ~ "mixed",
    has_mut           ~ "mutant",
    TRUE              ~ "wildtype"
  )
}

status_to_flag <- function(status, mixed_as_mutant = TRUE) {
  case_when(
    is.na(status)       ~ NA,
    status == "mutant"  ~ TRUE,
    status == "mixed"   ~ mixed_as_mutant,
    TRUE                ~ FALSE
  )
}

# Wilson 95% confidence interval for a proportion
add_ci <- function(df, z = 1.96) {
  df |>
    mutate(
      prevalence = carriers / n,
      ci_lower = (prevalence + z^2 / (2 * n) -
                    z * sqrt(prevalence * (1 - prevalence) / n + z^2 / (4 * n^2))) /
        (1 + z^2 / n),
      ci_upper = (prevalence + z^2 / (2 * n) +
                    z * sqrt(prevalence * (1 - prevalence) / n + z^2 / (4 * n^2))) /
        (1 + z^2 / n)
    )
}

summarise_prev <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(n = n(), carriers = sum(carrier), .groups = "drop") |>
    add_ci()
}

save_plot <- function(p, filename, width = 10, height = 6) {
  if (is.null(p)) return(invisible(NULL))
  print(p)
  ggsave(file.path(output_dir, filename), plot = p,
         width = width, height = height, units = "in", dpi = 300)
}


# ---- 4. Load Pf8 data -----------------------------------------------
samples_path  <- resolve_path(pf8_samples_file,  "Pf8 samples file")
genotype_path <- resolve_path(pf8_genotype_file, "Pf8 genotype file")

samples <- read_tsv(samples_path, show_col_types = FALSE, guess_max = 100000) |>
  clean_names()

# All genotype columns are read as text so mixed calls (e.g. "E,K") are kept
genotypes <- read_tsv(genotype_path, show_col_types = FALSE,
                      col_types = cols(.default = col_character())) |>
  rename(sample = Sample)

cat("\nSamples file:", nrow(samples), "rows\n")
cat("Genotype file:", nrow(genotypes), "rows\n")

geno <- genotypes |>
  inner_join(
    samples |>
      select(sample, country, admin_level_1,
             admin_level_1_latitude, admin_level_1_longitude,
             year, population, qc_pass),
    by = "sample"
  ) |>
  filter(toupper(as.character(qc_pass)) == "TRUE")

cat("Genotyped, QC-passed samples with metadata:", nrow(geno), "\n")
cat("Samples missing country or year:",
    sum(is.na(geno$country) | is.na(geno$year)), "\n")

# Dominant Pf8 population for each country (used for regions and colouring)
country_pop <- samples |>
  filter(!is.na(country), !is.na(population)) |>
  count(country, population) |>
  group_by(country) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(country, population)

sea_countries <- country_pop$country[country_pop$population %in% sea_populations]
east_africa_countries <-
  country_pop$country[country_pop$population %in% east_africa_populations]


# ---- 5. Define marker flags -----------------------------------------
# Wild-type and mutant alleles are set explicitly. The bracketed letter in
# the Pf8 column name is the 3D7 reference allele, which is NOT always the
# wild type (for example 3D7 carries the dhps 437G resistance allele).
# "NA" in the mutant column means "any allele other than wild type".
marker_spec <- tribble(
  ~marker,         ~column,              ~wild_type, ~mutant,       ~label,
  "crt_K76T",      "crt_76[K]",          "K",        "T",           "pfcrt K76T",
  "crt_76K",       "crt_76[K]",          "T",        "K",           "pfcrt K76 (wild type)",
  "crt_haplotype", "crt_72-76[CVMNK]",   "CVMNK",    NA_character_, "pfcrt mutant 72-76 haplotype",
  "mdr1_N86Y",     "mdr1_86[N]",         "N",        "Y",           "pfmdr1 N86Y",
  "mdr1_86N",      "mdr1_86[N]",         "Y",        "N",           "pfmdr1 N86 (wild type)",
  "mdr1_Y184F",    "mdr1_184[Y]",        "Y",        "F",           "pfmdr1 Y184F",
  "mdr1_D1246Y",   "mdr1_1246[D]",       "D",        "Y",           "pfmdr1 D1246Y",
  "mdr1_1246D",    "mdr1_1246[D]",       "Y",        "D",           "pfmdr1 1246D (wild type)",
  "dhfr_N51I",     "dhfr_51[N]",         "N",        "I",           "dhfr N51I",
  "dhfr_C59R",     "dhfr_59[C]",         "C",        "R",           "dhfr C59R",
  "dhfr_S108N",    "dhfr_108[S]",        "S",        "N",           "dhfr S108N",
  "dhfr_I164L",    "dhfr_164[I]",        "I",        "L",           "dhfr I164L",
  "dhps_A437G",    "dhps_437[G]",        "A",        "G",           "dhps A437G",
  "dhps_K540E",    "dhps_540[K]",        "K",        "E",           "dhps K540E",
  "dhps_A581G",    "dhps_581[A]",        "A",        "G",           "dhps A581G",
  "dhps_A613S",    "dhps_613[A]",        "A",        "S|T",         "dhps A613S/T"
)

for (i in seq_len(nrow(marker_spec))) {
  m <- marker_spec[i, ]
  if (!m$column %in% names(geno)) {
    warning("Column not found, skipping: ", m$column)
    next
  }
  geno[[m$marker]] <- status_to_flag(
    classify_marker(geno[[m$column]], m$wild_type, m$mutant),
    mixed_counts_as_mutant
  )
}

# kelch13: parse mutation names such as "C580Y", "a569s,v445i*", "c580y/a675v*"
k13_col <- "kelch13_349-726_ns_changes"
if (k13_col %in% names(geno)) {
  k13_raw     <- geno[[k13_col]]
  k13_missing <- (!is.na(k13_raw) & trimws(k13_raw) == "-") |
    (is.na(k13_raw) & !k13_blank_means_wildtype)
  k13_muts <- str_extract_all(
    replace_na(toupper(gsub("\\*", "", k13_raw)), ""),
    "[A-Z][0-9]+[A-Z]"
  )
  geno$k13_mutations <- k13_muts
  geno$k13_c580y     <- if_else(k13_missing, NA, map_lgl(k13_muts, \(m) "C580Y" %in% m))
  geno$k13_validated <- if_else(k13_missing, NA, map_lgl(k13_muts, \(m) any(m %in% k13_validated)))
  geno$k13_who_listed <- if_else(
    k13_missing, NA,
    map_lgl(k13_muts, \(m) any(m %in% c(k13_validated, k13_candidate)))
  )
} else {
  warning("kelch13 column not found; Part D and kelch13 analyses will be skipped.")
}

# Copy-number calls: 1 = duplicated, 0 = single copy, -1 = not called
dup_to_flag <- function(x) {
  v <- suppressWarnings(as.integer(x))
  if_else(is.na(v) | v < 0, NA, v >= 1)
}
if ("mdr1_dup_call" %in% names(geno)) geno$mdr1_dup <- dup_to_flag(geno$mdr1_dup_call)
if ("pm2_dup_call"  %in% names(geno)) geno$pm2_dup  <- dup_to_flag(geno$pm2_dup_call)

# Composite flags: each component must be called. With mixed infections these
# mean "carries the mutant allele at every site", not a confirmed haplotype.
all_called <- function(...) {
  cols <- list(...)
  any_na <- Reduce(`|`, lapply(cols, is.na))
  all_t  <- Reduce(`&`, cols)
  if_else(any_na, NA, all_t)
}

geno <- geno |>
  mutate(
    dhfr_triple  = all_called(dhfr_N51I, dhfr_C59R, dhfr_S108N),
    dhps_double  = all_called(dhps_A437G, dhps_K540E),
    sp_quintuple = all_called(dhfr_triple, dhps_double),
    mdr1_NFD     = all_called(mdr1_86N, mdr1_Y184F, mdr1_1246D)
  )

marker_labels <- c(
  setNames(marker_spec$label, marker_spec$marker),
  k13_c580y      = "kelch13 C580Y",
  k13_validated  = "kelch13 WHO-validated mutation",
  k13_who_listed = "kelch13 validated or candidate",
  mdr1_dup       = "pfmdr1 duplication",
  pm2_dup        = "plasmepsin 2/3 amplification",
  dhfr_triple    = "dhfr triple mutant (51I/59R/108N)",
  dhps_double    = "dhps double mutant (437G/540E)",
  sp_quintuple   = "SP quintuple mutant",
  mdr1_NFD       = "pfmdr1 NFD (86N/184F/1246D)"
)
flag_cols <- intersect(names(marker_labels), names(geno))
marker_labels <- marker_labels[flag_cols]

# Long format: one row per sample per marker (missing calls dropped)
geno_long <- geno |>
  select(sample, country, admin_level_1,
         admin_level_1_latitude, admin_level_1_longitude,
         year, population, all_of(flag_cols)) |>
  pivot_longer(all_of(flag_cols), names_to = "marker", values_to = "carrier") |>
  filter(!is.na(carrier)) |>
  mutate(marker_label = unname(marker_labels[marker]))

# Prevalence tables
prev_country_year <- geno_long |>
  filter(!is.na(country), !is.na(year)) |>
  summarise_prev(marker, marker_label, country, year)

prev_pop_year <- geno_long |>
  filter(!is.na(population), !is.na(year)) |>
  summarise_prev(marker, marker_label, population, year)

write_csv(prev_country_year, file.path(output_dir, "prevalence_by_country_year.csv"))
write_csv(prev_pop_year,     file.path(output_dir, "prevalence_by_population_year.csv"))

in_years <- function(x) between(x, summary_year_range[1], summary_year_range[2])

world <- ne_countries(scale = "medium", returnclass = "sf")


# =====================================================================
# PART A. Marker trends over time
# =====================================================================
plot_trend <- function(tab, marker_id, group_var, group_label, title,
                       min_n = min_n_trend) {
  d <- tab |> filter(marker == marker_id, n >= min_n)
  if (nrow(d) == 0) {
    message("No group-years with at least ", min_n, " samples for ", marker_id)
    return(NULL)
  }
  ggplot(d, aes(x = year, y = prevalence,
                color = .data[[group_var]], group = .data[[group_var]])) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = .data[[group_var]]),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.7) +
    geom_point(aes(size = n)) +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    scale_size_area(max_size = 3.5, name = "Samples") +
    labs(title = title,
         subtitle = paste0("Shaded band = 95% CI; only years with at least ",
                           min_n, " samples"),
         x = "Year", y = "Prevalence", color = group_label, fill = group_label) +
    theme_minimal(base_size = 12)
}

# kelch13 in Southeast Asia, by country
save_plot(
  plot_trend(prev_country_year |> filter(country %in% sea_countries),
             "k13_c580y", "country", "Country",
             "kelch13 C580Y prevalence, Southeast Asia"),
  "trend_k13_C580Y_southeast_asia.png"
)
save_plot(
  plot_trend(prev_country_year |> filter(country %in% sea_countries),
             "k13_validated", "country", "Country",
             "kelch13 WHO-validated mutations, Southeast Asia"),
  "trend_k13_validated_southeast_asia.png"
)

# dhps K540E in East Africa, by country
save_plot(
  plot_trend(prev_country_year |> filter(country %in% east_africa_countries),
             "dhps_K540E", "country", "Country",
             "dhps K540E prevalence, East Africa"),
  "trend_dhps_K540E_east_africa.png"
)

# pfcrt K76T by Pf8 population
save_plot(
  plot_trend(prev_pop_year, "crt_K76T", "population", "Population",
             "pfcrt K76T prevalence by Pf8 population"),
  "trend_crt_K76T_by_population.png"
)

# pfcrt K76T by country (all countries with enough data)
save_plot(
  plot_trend(prev_country_year, "crt_K76T", "country", "Country",
             "pfcrt K76T prevalence by country"),
  "trend_crt_K76T_by_country.png", width = 12, height = 7
)


# =====================================================================
# PART B. Admin-1 maps of marker prevalence
# =====================================================================
# Points sit at the admin-1 centroid supplied in the Pf8 samples file.
map_admin1 <- function(marker_id, xlim = NULL, ylim = NULL, region_label = "global") {
  d <- geno_long |>
    filter(marker == marker_id,
           !is.na(admin_level_1_latitude), !is.na(admin_level_1_longitude),
           !is.na(year), in_years(year)) |>
    summarise_prev(country, admin_level_1,
                   admin_level_1_latitude, admin_level_1_longitude) |>
    filter(n >= min_n_map) |>
    arrange(prevalence)
  
  if (nrow(d) == 0) {
    message("No admin-1 regions with at least ", min_n_map, " samples for ", marker_id)
    return(NULL)
  }
  
  yr_label <- if (all(is.infinite(summary_year_range))) "all years" else
    paste(summary_year_range, collapse = "-")
  
  ggplot() +
    geom_sf(data = world, fill = "grey95", color = "grey75", linewidth = 0.2) +
    geom_point(
      data = d,
      aes(x = admin_level_1_longitude, y = admin_level_1_latitude,
          color = prevalence, size = n),
      alpha = 0.85
    ) +
    scale_color_viridis_c(option = "C", limits = c(0, 1),
                          labels = percent, name = "Prevalence") +
    scale_size_area(max_size = 6, name = "Samples") +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE,
             default_crs = st_crs(4326)) +
    labs(title = paste0(unname(marker_labels[marker_id]), " prevalence by admin-1 region"),
         subtitle = paste0("Pf8, ", yr_label, "; regions with at least ",
                           min_n_map, " samples (", region_label, ")"),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 12)
}

crops <- list(
  southeast_asia = list(xlim = c(92, 112), ylim = c(5, 28)),
  africa         = list(xlim = c(-20, 52), ylim = c(-35, 20))
)

map_markers <- intersect(
  c("k13_validated", "k13_c580y", "dhps_K540E", "crt_K76T",
    "dhfr_triple", "sp_quintuple", "mdr1_NFD"),
  flag_cols
)

for (m in map_markers) {
  save_plot(map_admin1(m), paste0("map_admin1_", m, "_global.png"), 12, 6.5)
  
  crop_name <- if (startsWith(m, "k13")) "southeast_asia" else "africa"
  save_plot(
    map_admin1(m, crops[[crop_name]]$xlim, crops[[crop_name]]$ylim, crop_name),
    paste0("map_admin1_", m, "_", crop_name, ".png"), 9, 7
  )
}


# =====================================================================
# PART C. Compare Pf8 populations
# =====================================================================
key_markers <- intersect(
  c("k13_validated", "crt_K76T", "crt_haplotype", "mdr1_N86Y", "mdr1_Y184F",
    "mdr1_NFD", "dhfr_triple", "dhps_K540E", "dhps_A437G", "sp_quintuple",
    "mdr1_dup", "pm2_dup"),
  flag_cols
)

pop_heat <- geno_long |>
  filter(!is.na(population), !is.na(year), in_years(year),
         marker %in% key_markers) |>
  summarise_prev(marker, marker_label, population) |>
  filter(n >= min_n_map)

write_csv(pop_heat, file.path(output_dir, "prevalence_by_population.csv"))

p_heat <- ggplot(pop_heat, aes(x = marker_label, y = population, fill = prevalence)) +
  geom_tile(color = "white") +
  geom_text(aes(label = percent(prevalence, accuracy = 1)), size = 3) +
  scale_fill_viridis_c(option = "C", limits = c(0, 1), labels = percent,
                       name = "Prevalence") +
  labs(title = "Resistance-marker prevalence by Pf8 population",
       subtitle = paste0("Populations with at least ", min_n_map, " samples"),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

save_plot(p_heat, "population_marker_heatmap.png", 12, 6.5)

# Trends for several markers, one line per population
plot_pop_trends <- function(markers, title) {
  d <- prev_pop_year |> filter(marker %in% markers, n >= min_n_trend)
  if (nrow(d) == 0) return(NULL)
  ggplot(d, aes(x = year, y = prevalence, color = population, group = population)) +
    geom_line(linewidth = 0.6) +
    geom_point(aes(size = n)) +
    facet_wrap(~ marker_label, ncol = 2) +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    scale_size_area(max_size = 3, name = "Samples") +
    labs(title = title,
         subtitle = paste0("Population-years with at least ", min_n_trend, " samples"),
         x = "Year", y = "Prevalence", color = "Population") +
    theme_minimal(base_size = 11)
}

save_plot(
  plot_pop_trends(intersect(c("crt_K76T", "dhfr_triple", "dhps_K540E", "k13_validated"),
                            flag_cols),
                  "Marker trends by Pf8 population"),
  "population_marker_trends.png", 12, 8
)


# =====================================================================
# PART D. Spread of kelch13 mutations by year and country
# =====================================================================
if ("k13_validated" %in% names(geno)) {
  
  k13_ok <- geno |>
    filter(!is.na(k13_validated), !is.na(country), !is.na(year))
  
  # D1. Heatmap of validated-mutation prevalence, countries with any detection
  k13_cy <- prev_country_year |> filter(marker == "k13_validated")
  
  ever_detected <- k13_cy |>
    group_by(country) |>
    summarise(any_detected = sum(carriers) > 0, .groups = "drop") |>
    filter(any_detected) |>
    pull(country)
  
  k13_heat <- k13_cy |>
    filter(country %in% ever_detected, n >= min_n_trend) |>
    mutate(country = fct_reorder(country, prevalence, .fun = max))
  
  write_csv(k13_cy, file.path(output_dir, "k13_validated_by_country_year.csv"))
  
  if (nrow(k13_heat) > 0) {
    p_k13_heat <- ggplot(k13_heat, aes(x = factor(year), y = country, fill = prevalence)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "C", limits = c(0, 1), labels = percent,
                           name = "Prevalence") +
      labs(title = "kelch13 WHO-validated mutations by country and year",
           subtitle = paste0("Countries with at least one detection; cells with at least ",
                             min_n_trend, " samples"),
           x = "Year", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
            panel.grid = element_blank())
    save_plot(p_k13_heat, "k13_validated_heatmap_country_year.png", 12, 7)
  }
  
  # D2. Which mutations, where and when
  k13_n_cy <- k13_ok |> count(country, year, name = "n_k13")
  
  k13_carriers_long <- k13_ok |>
    select(sample, country, year, population, k13_mutations) |>
    unnest_longer(k13_mutations, values_to = "mutation") |>
    distinct(sample, mutation, .keep_all = TRUE)
  
  k13_mut_cy <- k13_carriers_long |>
    count(country, year, mutation, name = "carriers") |>
    left_join(k13_n_cy, by = c("country", "year")) |>
    mutate(prevalence = carriers / n_k13,
           who_status = case_when(mutation %in% k13_validated ~ "validated",
                                  mutation %in% k13_candidate ~ "candidate",
                                  TRUE ~ "other"))
  
  write_csv(k13_mut_cy, file.path(output_dir, "k13_mutations_by_country_year.csv"))
  
  # First and last year each validated mutation was seen in each country
  k13_first <- k13_carriers_long |>
    filter(mutation %in% k13_validated) |>
    group_by(country, mutation) |>
    summarise(first_year = min(year), last_year = max(year),
              samples = n(), .groups = "drop") |>
    arrange(country, first_year)
  
  write_csv(k13_first, file.path(output_dir, "k13_first_detection_by_country.csv"))
  print(k13_first, n = 40)
  
  plot_k13_mutations <- function(countries, title, top_n = 8) {
    d <- k13_mut_cy |>
      filter(country %in% countries, mutation %in% k13_validated,
             n_k13 >= min_n_trend)
    if (nrow(d) == 0) return(NULL)
    top <- d |>
      group_by(mutation) |>
      summarise(total = sum(carriers), .groups = "drop") |>
      slice_max(total, n = top_n, with_ties = FALSE) |>
      pull(mutation)
    d <- d |> filter(mutation %in% top)
    ggplot(d, aes(x = year, y = prevalence, color = mutation, group = mutation)) +
      geom_line(linewidth = 0.7) +
      geom_point(aes(size = n_k13)) +
      facet_wrap(~ country) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      scale_size_area(max_size = 3, name = "Samples") +
      labs(title = title,
           subtitle = paste0("Top ", top_n, " validated mutations; years with at least ",
                             min_n_trend, " samples"),
           x = "Year", y = "Prevalence", color = "Mutation") +
      theme_minimal(base_size = 11)
  }
  
  save_plot(
    plot_k13_mutations(sea_countries, "kelch13 mutations by year, Southeast Asia"),
    "k13_mutations_by_year_southeast_asia.png", 12, 7
  )
  
  other_countries <- setdiff(ever_detected, sea_countries)
  save_plot(
    plot_k13_mutations(other_countries, "kelch13 mutations by year, outside Southeast Asia"),
    "k13_mutations_by_year_other_regions.png", 12, 7
  )
}


# =====================================================================
# PART E. Link Pf8 marker prevalence to WHO TES outcomes
# =====================================================================
# This is an ecological comparison: WHO efficacy studies are matched to Pf8
# samples from the same country within +/- year_window years of the study.
# Pf8 samples are not the patients enrolled in the efficacy trials.
if (run_who_analysis && file.exists(who_file)) {
  
  who <- read_excel(who_file, sheet = who_sheet) |> clean_names()
  
  needed <- c("country_name", "drug_name", "plasmodium_species", "year_start")
  if (!all(needed %in% names(who))) {
    stop("WHO file is missing columns: ",
         paste(setdiff(needed, names(who)), collapse = ", "))
  }
  
  num <- function(x) suppressWarnings(parse_number(as.character(x)))
  pct <- function(x) { v <- num(x); if_else(between(v, 0, 100), v, NA_real_) }
  
  day3_col <- if ("positive_day_3_days" %in% names(who)) {
    "positive_day_3_days"
  } else {
    cand <- names(who)[str_detect(names(who), "positive.*day.?3|day.?3.*positive")]
    if (length(cand) == 1) cand else NA_character_
  }
  cat("\nDay-3 positivity column used:", ifelse(is.na(day3_col), "none found", day3_col), "\n")
  
  # Spelling differences between WHO and Pf8 country names
  country_fix <- c(
    "Viet Nam" = "Vietnam",
    "Lao People's Democratic Republic" = "Laos",
    "Lao PDR" = "Laos",
    "United Republic of Tanzania" = "Tanzania",
    "Cote d'Ivoire" = "C\u00f4te d'Ivoire",
    "C\u00f4te d\u2019Ivoire" = "C\u00f4te d'Ivoire",
    "The Gambia" = "Gambia",
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Congo, Democratic Republic of the" = "Democratic Republic of the Congo",
    "DR Congo" = "Democratic Republic of the Congo"
  )
  harmonise_country <- function(x) {
    x <- str_squish(as.character(x))
    coalesce(unname(country_fix[x]), x)
  }
  
  # Drug groups: edit these patterns if the drug names in your file differ
  drug_defs <- list(
    ACT = list(include = "artemether|artesunate|artemisinin|dihydroartemisinin|\\bDHA\\b|artenimol",
               exclude = NA_character_),
    AL  = list(include = "artemether", exclude = "amodiaquine|artesunate|mefloquine"),
    CQ  = list(include = "chloroquine", exclude = "amodiaquine|hydroxychloroquine"),
    SP  = list(include = "sulfadoxine|\\bSP\\b",
               exclude = "artesunate|artemether|amodiaquine|mefloquine|chlorproguanil|\\+")
  )
  drug_match <- function(x, include, exclude) {
    inc <- str_detect(x, regex(include, ignore_case = TRUE))
    if (is.na(exclude)) inc else inc & !str_detect(x, regex(exclude, ignore_case = TRUE))
  }
  
  who_clean <- who |>
    mutate(
      who_row    = row_number(),
      country    = harmonise_country(country_name),
      year_start = num(year_start),
      year_end   = if ("year_end" %in% names(who)) num(year_end) else year_start,
      mid_year   = round((year_start + coalesce(year_end, year_start)) / 2),
      failure_pp = if ("treatment_failure_pp" %in% names(who)) pct(treatment_failure_pp) else NA_real_,
      failure_km = if ("treatment_failure_km" %in% names(who)) pct(treatment_failure_km) else NA_real_,
      day3_pos   = if (!is.na(day3_col)) pct(.data[[day3_col]]) else NA_real_
    ) |>
    filter(str_detect(str_to_lower(plasmodium_species), "falciparum"),
           !is.na(country), !is.na(mid_year), !is.na(drug_name))
  
  for (key in names(drug_defs)) {
    who_clean[[paste0("is_", key)]] <-
      drug_match(who_clean$drug_name, drug_defs[[key]]$include, drug_defs[[key]]$exclude)
    cat("\nWHO drug names matched to", key, ":\n")
    print(who_clean |> filter(.data[[paste0("is_", key)]]) |> count(drug_name, sort = TRUE),
          n = 15)
  }
  
  unmatched <- setdiff(unique(who_clean$country), unique(geno$country))
  cat("\nWHO countries with no Pf8 samples (not linkable):\n")
  print(sort(unmatched))
  
  pf8_cy <- prev_country_year |> select(marker, country, year, n, carriers)
  
  link_who_pf8 <- function(who_df, marker_id) {
    pf8_m <- pf8_cy |> filter(marker == marker_id)
    who_df |>
      select(who_row, country, mid_year) |>
      inner_join(pf8_m, by = "country", relationship = "many-to-many") |>
      filter(abs(year - mid_year) <= year_window) |>
      group_by(who_row) |>
      summarise(pf8_n = sum(n), pf8_carriers = sum(carriers), .groups = "drop") |>
      filter(pf8_n >= min_n_link) |>
      mutate(prevalence = pf8_carriers / pf8_n) |>
      left_join(who_df |> select(-country, -mid_year), by = "who_row") |>
      left_join(who_df |> select(who_row, country, mid_year), by = "who_row") |>
      left_join(country_pop |> rename(pf8_population = population), by = "country")
  }
  
  spearman_row <- function(x, y, level) {
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok]; y <- y[ok]
    if (length(x) < 5 || n_distinct(x) < 3 || n_distinct(y) < 3) {
      return(tibble(level = level, n_units = length(x), rho = NA_real_, p_value = NA_real_))
    }
    ct <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
    tibble(level = level, n_units = length(x),
           rho = unname(ct$estimate), p_value = ct$p.value)
  }
  
  plot_link <- function(d, y, ylab, title, subtitle, hline = NULL) {
    d <- d |> filter(!is.na(.data[[y]])) |> mutate(prev_pct = prevalence * 100)
    p <- ggplot(d, aes(x = prev_pct, y = .data[[y]], size = pf8_n, color = pf8_population)) +
      geom_point(alpha = 0.65) +
      geom_smooth(aes(x = prev_pct, y = .data[[y]]), inherit.aes = FALSE,
                  method = "lm", formula = y ~ x, se = FALSE,
                  color = "grey30", linewidth = 0.6) +
      scale_size_area(max_size = 5, name = "Pf8 samples\nmatched") +
      labs(title = title, subtitle = subtitle,
           x = "Marker prevalence in Pf8 samples (%)", y = ylab,
           color = "Pf8 population") +
      theme_minimal(base_size = 12)
    if (!is.null(hline)) p <- p + geom_hline(yintercept = hline, linetype = "dashed")
    p
  }
  
  # Each row: a marker compared with an outcome for one drug group
  pair_specs <- tribble(
    ~analysis,                                  ~drug_key, ~marker,         ~outcome,     ~outcome_label,
    "kelch13 vs day-3 positivity (ACTs)",       "ACT",     "k13_validated", "day3_pos",   "Day-3 positivity (%)",
    "kelch13 vs treatment failure (ACTs)",      "ACT",     "k13_validated", "failure_pp", "Per-protocol treatment failure (%)",
    "kelch13 vs treatment failure (AL)",        "AL",      "k13_validated", "failure_pp", "Per-protocol treatment failure (%)",
    "pfcrt haplotype vs chloroquine failure",   "CQ",      "crt_haplotype", "failure_pp", "Per-protocol treatment failure (%)",
    "pfcrt K76T vs chloroquine failure",        "CQ",      "crt_K76T",      "failure_pp", "Per-protocol treatment failure (%)",
    "pfmdr1 N86 vs AL failure",                 "AL",      "mdr1_86N",      "failure_pp", "Per-protocol treatment failure (%)",
    "pfmdr1 NFD vs AL failure",                 "AL",      "mdr1_NFD",      "failure_pp", "Per-protocol treatment failure (%)",
    "pfcrt K76 (wild type) vs AL failure",      "AL",      "crt_76K",       "failure_pp", "Per-protocol treatment failure (%)",
    "dhfr triple mutant vs SP failure",         "SP",      "dhfr_triple",   "failure_pp", "Per-protocol treatment failure (%)",
    "dhps K540E vs SP failure",                 "SP",      "dhps_K540E",    "failure_pp", "Per-protocol treatment failure (%)",
    "SP quintuple mutant vs SP failure",        "SP",      "sp_quintuple",  "failure_pp", "Per-protocol treatment failure (%)"
  )
  
  results <- list()
  linked_store <- list()
  
  for (i in seq_len(nrow(pair_specs))) {
    sp <- pair_specs[i, ]
    
    if (!sp$marker %in% pf8_cy$marker) {
      message("Skipping '", sp$analysis, "': marker not available in Pf8 data.")
      next
    }
    
    who_sub <- who_clean[who_clean[[paste0("is_", sp$drug_key)]], ]
    linked  <- link_who_pf8(who_sub, sp$marker)
    
    if (nrow(linked) == 0 || all(is.na(linked[[sp$outcome]]))) {
      message("Skipping '", sp$analysis, "': no linkable WHO records with outcome data.")
      next
    }
    
    rec <- spearman_row(linked$prevalence, linked[[sp$outcome]], "study record")
    
    by_country <- linked |>
      filter(!is.na(.data[[sp$outcome]])) |>
      group_by(country) |>
      summarise(prevalence = mean(prevalence),
                outcome = median(.data[[sp$outcome]]), .groups = "drop")
    cty <- spearman_row(by_country$prevalence, by_country$outcome, "country")
    
    results[[i]] <- bind_rows(rec, cty) |>
      mutate(analysis = sp$analysis, marker = sp$marker, drug_group = sp$drug_key,
             .before = 1)
    linked_store[[sp$analysis]] <- linked |> mutate(analysis = sp$analysis)
    
    subtitle <- if (is.na(rec$rho)) {
      paste0(rec$n_units, " study records (too few for a correlation)")
    } else {
      sprintf("Spearman rho = %.2f (p = %.3g); %d study records in %d countries",
              rec$rho, rec$p_value, rec$n_units, n_distinct(linked$country))
    }
    
    p <- plot_link(
      linked, sp$outcome, sp$outcome_label,
      title = sp$analysis, subtitle = subtitle,
      hline = if (sp$outcome == "failure_pp") 10 else NULL
    )
    
    file_stub <- str_replace_all(str_to_lower(sp$analysis), "[^a-z0-9]+", "_")
    save_plot(p, paste0("link_", file_stub, ".png"), 9, 6)
  }
  
  correlation_summary <- bind_rows(results)
  
  if (nrow(correlation_summary) > 0) {
    cat("\nPf8 marker prevalence vs WHO TES outcomes (Spearman):\n")
    print(correlation_summary, n = Inf)
    write_csv(correlation_summary, file.path(output_dir, "who_pf8_correlations.csv"))
    write_csv(bind_rows(linked_store) |>
                select(analysis, who_row, country, mid_year, prevalence, pf8_n,
                       any_of(c("drug_name", "failure_pp", "failure_km", "day3_pos"))),
              file.path(output_dir, "who_pf8_linked_records.csv"))
  } else {
    message("No WHO-Pf8 comparisons could be made. Check country names, ",
            "drug names and the day-3 column.")
  }
  
} else {
  message("\nWHO file not found or run_who_analysis is FALSE; skipping Part E.")
}


# ---- Finished -------------------------------------------------------
cat("\n====================================\n")
cat("ANALYSIS COMPLETE\n")
cat("Results saved in:", normalizePath(output_dir), "\n")
cat("====================================\n")
print(list.files(output_dir))