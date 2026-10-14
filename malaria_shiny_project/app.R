# Malaria elimination evidence explorer — Shiny + Leaflet
# Palette requested by project: #c2168c, #f187d0, #0c0434, #8669a2, #890f63, #312c54
# IMPORTANT: This is an evidence explorer, not a causal model. Missing values are never recoded to zero.

required_packages <- c("shiny", "leaflet", "sf", "dplyr", "readr", "stringr", "ggplot2", "DT", "scales", "htmltools", "rnaturalearth", "rnaturalearthdata")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install these packages before running the app: install.packages(c(",
       paste(sprintf("'%s'", missing_packages), collapse = ", "), "))")
}

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(DT)
library(scales)
library(htmltools)

CASE_FILE <- file.path("data", "who_europe_malaria_indigenous_cases_2000_2015_audited.csv")
EVENT_FILE <- file.path("data", "azerbaijan_malaria_interventions_financing_2000_2015_audited.csv")
POP_FILE <- file.path("data", "population_denominators.csv")
INSURANCE_FILE <- file.path("data", "health_insurance_coverage.csv")

if (!file.exists(CASE_FILE)) stop("Missing ", CASE_FILE, ". Keep app.R and the data/ folder together.")
if (!file.exists(EVENT_FILE)) stop("Missing ", EVENT_FILE, ". Keep app.R and the data/ folder together.")

cases <- read_csv(CASE_FILE, show_col_types = FALSE, na = c("", "NA", "NaN"))
events <- read_csv(EVENT_FILE, show_col_types = FALSE, na = c("", "NA", "NaN"))

SOURCE_REGISTER_FILE <- file.path("data", "source_validation_register.csv")
source_register <- if (file.exists(SOURCE_REGISTER_FILE)) read_csv(SOURCE_REGISTER_FILE, show_col_types = FALSE, na = c("", "NA", "NaN")) else tibble::tibble(note = "Source validation register not found.")

# Basic schema checks; these stop the app instead of silently plotting malformed data.
case_required <- c("country", "iso3", "map_key_iso3", "year", "source_reported_cases", "map_value_cases", "qa_status", "source_url")
if (!all(case_required %in% names(cases))) stop("Case CSV is missing required columns: ", paste(setdiff(case_required, names(cases)), collapse = ", "))
event_required <- c("year_start", "year_end", "country_or_countries", "iso3_or_iso3_list", "category", "event_or_intervention", "description", "evidence_status", "source_url")
if (!all(event_required %in% names(events))) stop("Intervention CSV is missing required columns: ", paste(setdiff(event_required, names(events)), collapse = ", "))

cases <- cases |>
  mutate(
    year = as.integer(year),
    map_key_iso3 = str_to_upper(str_trim(as.character(map_key_iso3))),
    source_reported_cases = suppressWarnings(as.numeric(source_reported_cases)),
    map_value_cases = suppressWarnings(as.numeric(map_value_cases)),
    qa_status = coalesce(as.character(qa_status), "not_specified")
  )

if (!"population" %in% names(cases)) cases$population <- NA_real_
if (!"population_source_citation" %in% names(cases)) cases$population_source_citation <- NA_character_
if (!"population_source_url" %in% names(cases)) cases$population_source_url <- NA_character_

# Optional population denominator file: add real, cited population observations to enable rates.
if (file.exists(POP_FILE)) {
  pop <- read_csv(POP_FILE, show_col_types = FALSE, na = c("", "NA", "NaN"))
  if (all(c("iso3", "year", "population") %in% names(pop))) {
    pop <- pop |>
      mutate(iso3 = str_to_upper(str_trim(as.character(iso3))), year = as.integer(year), population = suppressWarnings(as.numeric(population)))
    if (!"population_source_citation" %in% names(pop)) pop$population_source_citation <- NA_character_
    if (!"population_source_url" %in% names(pop)) pop$population_source_url <- NA_character_
    if (anyDuplicated(pop[c("iso3", "year")])) stop("Population file has duplicate ISO3-year rows; resolve before rates are used.")
    pop <- pop |> distinct(iso3, year, .keep_all = TRUE) |>
      transmute(iso3, year, population_joined = population,
                population_citation_joined = as.character(population_source_citation),
                population_url_joined = as.character(population_source_url))
    cases <- cases |>
      left_join(pop, by = c("iso3", "year")) |>
      mutate(population = coalesce(as.numeric(population), as.numeric(population_joined)),
             population_source_citation = coalesce(as.character(population_source_citation), population_citation_joined),
             population_source_url = coalesce(as.character(population_source_url), population_url_joined),
             indigenous_cases_per_100k = if_else(!is.na(map_value_cases) & !is.na(population) & population > 0, map_value_cases / population * 100000, NA_real_),
             rate_status = if_else(!is.na(indigenous_cases_per_100k), "calculated_from_joined_population", "population_denominator_missing_or_case_value_unresolved")) |>
      select(-any_of(c("population_joined", "population_citation_joined", "population_url_joined")))
  }
}
if (!"indigenous_cases_per_100k" %in% names(cases)) cases$indigenous_cases_per_100k <- NA_real_
if (!"population" %in% names(cases)) cases$population <- NA_real_
if (!"rate_status" %in% names(cases)) cases$rate_status <- "population_denominator_not_joined"

# Optional insurance data: displayed as a separate indicator and never treated as malaria intervention coverage.
insurance <- NULL
if (file.exists(INSURANCE_FILE)) {
  insurance_try <- try(read_csv(INSURANCE_FILE, show_col_types = FALSE, na = c("", "NA", "NaN")), silent = TRUE)
  if (!inherits(insurance_try, "try-error") && all(c("iso3", "year", "coverage_percent") %in% names(insurance_try))) {
    insurance <- insurance_try |>
      mutate(iso3 = str_to_upper(str_trim(as.character(iso3))), year = as.integer(year), coverage_percent = suppressWarnings(as.numeric(coverage_percent)))
    if (anyDuplicated(insurance[c("iso3", "year")])) stop("Insurance file has duplicate ISO3-year rows; resolve before display.")
    insurance <- insurance |> distinct(iso3, year, .keep_all = TRUE)
    insurance_join <- insurance |>
      select(iso3, year, coverage_percent, any_of(c("indicator_name", "population_definition", "insurance_type", "source_citation", "source_url", "source_edition", "retrieval_date", "verification_status"))) |>
      rename(insurance_coverage_percent = coverage_percent,
             insurance_source_citation = any_of("source_citation"),
             insurance_source_url = any_of("source_url"),
             insurance_source_edition = any_of("source_edition"),
             insurance_retrieval_date = any_of("retrieval_date"),
             insurance_verification_status = any_of("verification_status"))
    cases <- cases |> left_join(insurance_join, by = c("iso3", "year"))
  }
}
if (!"insurance_coverage_percent" %in% names(cases)) cases$insurance_coverage_percent <- NA_real_
for (nm in c("insurance_source_citation", "insurance_source_url", "insurance_source_edition", "insurance_retrieval_date", "insurance_verification_status", "indicator_name", "population_definition", "insurance_type")) {
  if (!nm %in% names(cases)) cases[[nm]] <- NA_character_
}

# Case file QA summary. These checks are visible in the app and should be reviewed before publication.
duplicate_case_rows <- cases |> count(map_key_iso3, year) |> filter(n > 1)
negative_case_rows <- cases |> filter(!is.na(source_reported_cases) & source_reported_cases < 0)
invalid_year_rows <- cases |> filter(is.na(year) | year < 2000 | year > 2015)
if (nrow(duplicate_case_rows) > 0) stop("Duplicate ISO3-year case rows found. Resolve them with prepare_data.R before running the app.")
if (nrow(negative_case_rows) > 0) warning("Negative case counts found. Review source data.")
if (nrow(invalid_year_rows) > 0) warning("Rows outside 2000–2015 found. Review the selected study period.")

# Natural Earth country boundaries are used only as map geometry. Record package version when publishing.
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(4326)
if (!"iso_a3" %in% names(world)) stop("The boundary data do not contain iso_a3; inspect your boundary source.")
world <- world |> mutate(map_key_iso3 = str_to_upper(as.character(iso_a3)))

palette_requested <- c("#c2168c", "#f187d0", "#0c0434", "#8669a2", "#890f63", "#312c54")
zero_color <- "#f7f5f8"
missing_color <- "#b8b8b8"
conflict_color <- "#f3c969"

# Fixed bins aid year-to-year comparison. Zero is handled separately; missing/unresolved is grey/amber.
case_bins <- c(0, 1, 5, 20, 100, 1000, Inf)
rate_bins <- c(0, 0.1, 1, 5, 10, 50, Inf)

format_num <- function(x, digits = 2) {
  ifelse(is.na(x), "Not available", format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE))
}

# Country names are drawn from the boundary data. Case rows are joined by ISO3, not country-name strings.
case_years <- sort(unique(cases$year[!is.na(cases$year)]))
if (!length(case_years)) case_years <- 2000:2015
country_choices <- cases |> distinct(map_key_iso3, country) |> filter(!is.na(map_key_iso3), !is.na(country)) |> arrange(country)

ui <- fluidPage(
  tags$head(tags$style(HTML("\n    body { color: #0c0434; }\n    .app-title { color: #0c0434; font-weight: 700; margin-bottom: 0; }\n    .subtitle { color: #514b65; margin-bottom: 16px; }\n    .metric-card { border-left: 5px solid #c2168c; background: #faf8fb; padding: 12px 14px; margin-bottom: 10px; min-height: 88px; }\n    .metric-value { font-size: 25px; font-weight: 700; color: #0c0434; }\n    .metric-label { color: #514b65; font-size: 12px; text-transform: uppercase; letter-spacing: .04em; }\n    .note { background: #fbf5fa; border-left: 4px solid #8669a2; padding: 10px 12px; margin: 10px 0; }\n    .warning-note { background: #fff8e5; border-left: 4px solid #b88713; padding: 10px 12px; margin: 10px 0; }\n    .small-muted { color: #666; font-size: 12px; }\n  "))),
  div(class = "container-fluid",
      h2(class = "app-title", "Malaria elimination evidence explorer"),
      p(class = "subtitle", "WHO European Region · Indigenous malaria cases, interventions and financing · 2000–2015"),
      div(class = "note", HTML("<b>How to read this page:</b> the map shows reported indigenous cases where a displayable value is available. Grey indicates missing data; amber indicates a known unresolved value. Timing of interventions and declining cases can be compared, but timing alone does not prove causation.")),
      tabsetPanel(id = "main_tabs",
        tabPanel("Interactive map",
          sidebarLayout(
            sidebarPanel(width = 3,
              sliderInput("year", "Year", min = min(case_years), max = max(case_years), value = min(2012, max(case_years)), step = 1, sep = ""),
              radioButtons("indicator", "Map indicator", choices = c("Indigenous case count" = "cases", "Indigenous cases per 100,000" = "rate", "Health-insurance coverage (%)" = "insurance"), selected = "cases"),
              checkboxInput("show_all_labels", "Show country labels on hover", TRUE),
              actionButton("reset_view", "Reset map view"),
              hr(),
              h4("Selected year summary"),
              uiOutput("summary_cards"),
              p(class = "small-muted", "Rates are enabled only when a cited annual population denominator has been added to data/population_denominators.csv."),
              p(class = "small-muted", "Health-insurance coverage, if supplied, is displayed separately and is not used as a proxy for malaria prevention." )
            ),
            mainPanel(width = 9,
              leafletOutput("case_map", height = "650px"),
              h4("Country records for selected year"),
              DTOutput("year_table")
            )
          )
        ),
        tabPanel("Azerbaijan timeline",
          fluidRow(column(12, p("The timeline includes documented programme activities and funding milestones. Events may span multiple years; dates before 2000 are retained where needed to show programme context."))),
          plotOutput("timeline_plot", height = "450px"),
          h4("Intervention and financing records"),
          DTOutput("event_table"),
          div(class = "warning-note", "Funding amounts are not annual expenditure unless explicitly documented as such. Missing coverage or insurance figures are not zero.")
        ),
        tabPanel("Country detail",
          sidebarLayout(
            sidebarPanel(width = 3,
              selectInput("country_iso", "Country", choices = setNames(country_choices$map_key_iso3, paste0(country_choices$country, " (", country_choices$map_key_iso3, ")")), selected = "AZE"),
              p(class = "small-muted", "Country list uses ISO3 codes. The detail panel preserves source-reported values and their QA status.")
            ),
            mainPanel(width = 9,
              uiOutput("country_summary"),
              plotOutput("country_trend", height = "320px"),
              h4("Annual records"), DTOutput("country_table"),
              h4("Documented Azerbaijan intervention records"), DTOutput("country_events")
            )
          )
        ),
        tabPanel("Data quality & sources",
          h4("Data quality checks"),
          p("The application does not independently certify the original WHO compilation. The source validation register distinguishes partial corroboration from unresolved or unverified records. Verify all values against a single dated WHO data release before making definitive claims."),
          DTOutput("qa_summary"),
          h4("Rows requiring attention"), DTOutput("qa_rows"),
          h4("Source-by-source validation register"), DTOutput("source_register"),
          h4("Geographic join check (ISO3 codes not matched to boundary data)"), DTOutput("geo_qa"),
          h4("Sources and methods"),
          tags$ul(
            tags$li(tags$a(href = "https://apps.who.int/gho/athena/data/GHO/MALARIA_INDIG_STATUS%2CMALARIA_INDIG.html?ead=&filter=COUNTRY%3A%2A&profile=ztable", target = "_blank", "WHO Global Health Observatory — indigenous malaria cases")),
            tags$li(tags$a(href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC5201226/", target = "_blank", "Mammadov et al. (2016), Elimination of Plasmodium vivax Malaria in Azerbaijan")),
            tags$li(tags$a(href = "https://www.who.int/teams/global-malaria-programme/reports/world-malaria-report", target = "_blank", "WHO World Malaria Reports")),
            tags$li(tags$a(href = "https://www.naturalearthdata.com/", target = "_blank", "Natural Earth country boundary data"))
          ),
          p(class = "small-muted", paste0("Boundary package version: rnaturalearth ", as.character(utils::packageVersion("rnaturalearth")), ". Confirm the boundary version and ISO3 joins before deployment.")),
          p(class = "small-muted", "The data files include source and QA columns. Blank source-retrieval dates mean the historical compilation's original retrieval date was not recorded; do not substitute today's date for it.")
        )
      )
  )
)

server <- function(input, output, session) {
  selected_cases <- reactive({
    cases |> filter(year == input$year)
  })

  selected_map <- reactive({
    d <- selected_cases()
    joined <- world |> left_join(d, by = "map_key_iso3")
    joined |> mutate(
      map_metric_value = case_when(input$indicator == "cases" ~ map_value_cases,
                                   input$indicator == "rate" ~ indigenous_cases_per_100k,
                                   input$indicator == "insurance" ~ insurance_coverage_percent,
                                   TRUE ~ NA_real_),
      unresolved_conflict = input$indicator == "cases" & !is.na(qa_status) & str_detect(qa_status, "conflict|discrepancy"),
      map_fill = case_when(
        unresolved_conflict ~ conflict_color,
        is.na(map_metric_value) ~ missing_color,
        map_metric_value == 0 ~ zero_color,
        TRUE ~ NA_character_
      )
    )
  })

  output$case_map <- renderLeaflet({
    map_data <- selected_map()
    vals <- map_data$map_metric_value
    vals_pos <- vals[!is.na(vals) & vals > 0 & !map_data$unresolved_conflict]
    bins <- if (input$indicator == "cases") case_bins else if (input$indicator == "rate") rate_bins else c(0, 20, 40, 60, 80, 95, 100.0001)
    pal <- colorBin(palette = palette_requested, domain = if (length(vals_pos)) vals_pos else c(1), bins = bins, na.color = missing_color, right = FALSE)
    legend_vals <- if (length(vals_pos)) vals_pos else 1
    map_data$fill_final <- ifelse(!is.na(map_data$map_fill), map_data$map_fill, pal(map_data$map_metric_value))
    label_text <- if (input$indicator == "cases") {
      paste0("<b>", htmlEscape(map_data$name), "</b><br>Year: ", input$year,
             "<br>Indigenous cases (map value): ", ifelse(is.na(map_data$map_value_cases), "Not available / unresolved", format_num(map_data$map_value_cases, 0)),
             "<br>Source-reported cases: ", ifelse(is.na(map_data$source_reported_cases), "Not available", format_num(map_data$source_reported_cases, 0)),
             "<br>QA: ", htmlEscape(coalesce(map_data$qa_status, "no matching country-year record")))
    } else if (input$indicator == "rate") {
      paste0("<b>", htmlEscape(map_data$name), "</b><br>Year: ", input$year,
             "<br>Indigenous cases per 100,000: ", format_num(map_data$indigenous_cases_per_100k, 2),
             "<br>Population denominator: ", format_num(map_data$population, 0),
             "<br>Rate status: ", htmlEscape(coalesce(map_data$rate_status, "not available")))
    } else {
      paste0("<b>", htmlEscape(map_data$name), "</b><br>Year: ", input$year,
             "<br>Health-insurance coverage: ", format_num(map_data$insurance_coverage_percent, 1), "%",
             "<br>Note: insurance coverage is a separate health-system indicator, not malaria intervention coverage.")
    }
    leaflet(map_data, options = leafletOptions(minZoom = 2, worldCopyJump = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = map_data$fill_final, fillOpacity = 0.82, color = "#ffffff", weight = 0.65,
        opacity = 1, smoothFactor = 0.2,
        label = if (isTRUE(input$show_all_labels)) lapply(label_text, HTML) else NULL,
        highlightOptions = highlightOptions(weight = 2, color = "#0c0434", bringToFront = TRUE),
        layerId = ~map_key_iso3,
        popup = lapply(label_text, HTML)
      ) |>
      addLegend(position = "bottomright", pal = pal, values = legend_vals,
                title = if (input$indicator == "cases") "Positive case counts" else if (input$indicator == "rate") "Cases per 100,000" else "Insurance coverage (%)", opacity = 0.9) |>
      addLegend(position = "bottomleft", colors = c(zero_color, missing_color, conflict_color),
                labels = if (input$indicator == "cases") c("0 indigenous cases", "Missing or unavailable", "Unresolved source conflict") else if (input$indicator == "rate") c("0 cases per 100,000", "Missing or unavailable", "Unresolved case source conflict") else c("0% insurance coverage", "Missing or unavailable", "Not applicable / unresolved case conflict"),
                title = "Data status", opacity = 0.9) |>
      setView(lng = 30, lat = 53, zoom = 3)
  })

  observeEvent(input$reset_view, {
    leafletProxy("case_map") |> setView(lng = 30, lat = 53, zoom = 3)
  })

  output$summary_cards <- renderUI({
    d <- selected_cases()
    n_reported <- sum(!is.na(d$map_value_cases))
    n_zero <- sum(d$map_value_cases == 0, na.rm = TRUE)
    total_cases <- sum(d$map_value_cases, na.rm = TRUE)
    n_unresolved <- sum(is.na(d$map_value_cases) & !is.na(d$source_reported_cases))
    tagList(
      div(class = "metric-card", div(class = "metric-label", "Reported map values"), div(class = "metric-value", n_reported)),
      div(class = "metric-card", div(class = "metric-label", "Reported zeros"), div(class = "metric-value", n_zero)),
      div(class = "metric-card", div(class = "metric-label", "Sum of available map values"), div(class = "metric-value", format(total_cases, big.mark = ","))),
      div(class = "metric-card", div(class = "metric-label", "Values withheld / unresolved"), div(class = "metric-value", n_unresolved))
    )
  })

  output$year_table <- renderDT({
    selected_cases() |>
      select(country, map_key_iso3, year, source_reported_cases, map_value_cases, indigenous_cases_per_100k, insurance_coverage_percent, qa_status, verification_status, source_validation_status, source_validation_note, source_url) |>
      arrange(country) |>
      datatable(rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })

  aze_events <- reactive({
    events |> filter(str_detect(str_to_upper(coalesce(iso3_or_iso3_list, "")), "AZE")) |>
      mutate(year_start = as.integer(year_start), year_end = as.integer(year_end),
             plot_start = pmax(year_start, 2000, na.rm = FALSE),
             plot_end = if_else(is.na(year_end), plot_start, pmin(year_end, 2015, na.rm = FALSE))) |>
      filter(!is.na(year_start), year_start <= 2015, is.na(year_end) | year_end >= 2000) |>
      arrange(year_start, category, event_or_intervention)
  })

  output$timeline_plot <- renderPlot({
    ev <- aze_events()
    if (!nrow(ev)) return(ggplot() + theme_void() + labs(title = "No intervention records available for this period"))
    ev <- ev |> mutate(event_label = str_wrap(event_or_intervention, 38), event_label = factor(event_label, levels = rev(unique(event_label))))
    ggplot(ev, aes(y = event_label)) +
      geom_segment(aes(x = plot_start, xend = plot_end, yend = event_label, color = category), linewidth = 5, lineend = "round") +
      geom_point(aes(x = plot_start, color = category), size = 2.5) +
      scale_x_continuous(breaks = seq(2000, 2015, 1), limits = c(2000, 2015), expand = expansion(mult = c(0.01, 0.02))) +
      scale_color_manual(values = rep(palette_requested, length.out = length(unique(ev$category)))) +
      labs(x = "Year", y = NULL, color = "Category", title = "Documented Azerbaijan malaria-control milestones", subtitle = "Bars show reported programme periods; they do not establish causal effects") +
      theme_minimal(base_size = 12) + theme(axis.text.y = element_text(size = 9), panel.grid.minor = element_blank(), legend.position = "bottom")
  })

  output$event_table <- renderDT({
    aze_events() |>
      select(year_start, year_end, category, event_or_intervention, description, quantity_numeric, quantity_unit, geographic_or_population_scope, evidence_status, financial_measure_type, annual_amount_usd, actual_annual_expenditure_usd, insurance_indicator_status, source_citation, source_url, interpretation_limit, data_quality_note) |>
      datatable(rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })

  selected_country_rows <- reactive({ cases |> filter(map_key_iso3 == input$country_iso) |> arrange(year) })

  output$country_summary <- renderUI({
    d <- selected_country_rows()
    if (!nrow(d)) return(p("No country-year records found for this ISO3 code."))
    country_name <- d$country[which(!is.na(d$country))[1]]
    last_year_with_value <- if (any(!is.na(d$map_value_cases))) max(d$year[!is.na(d$map_value_cases)]) else NA_integer_
    div(class = "note", tags$b(country_name), br(),
        paste0("Records in selected dataset: ", nrow(d), ". "),
        paste0("Latest year with a displayable case value: ", ifelse(is.na(last_year_with_value), "none", last_year_with_value), ". "),
        "A blank display value can indicate missing information or an unresolved discrepancy, not zero cases.")
  })

  output$country_trend <- renderPlot({
    d <- selected_country_rows()
    ggplot(d, aes(x = year, y = map_value_cases)) +
      geom_line(color = "#8669a2", linewidth = 0.9, na.rm = TRUE) +
      geom_point(aes(color = qa_status), size = 2.5, na.rm = TRUE) +
      scale_color_manual(values = rep(palette_requested, length.out = max(1, length(unique(d$qa_status)))), guide = "none") +
      scale_x_continuous(breaks = seq(2000, 2015, 1)) +
      labs(x = "Year", y = "Indigenous cases (map-ready value)", title = "Annual indigenous malaria case trend", subtitle = "Unresolved values are left blank") +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
  })

  output$country_table <- renderDT({
    selected_country_rows() |>
      select(country, year, source_reported_cases, map_value_cases, population, indigenous_cases_per_100k, population_source_citation, population_source_url, insurance_coverage_percent, indicator_name, population_definition, insurance_type, insurance_source_citation, insurance_source_url, insurance_source_edition, insurance_retrieval_date, insurance_verification_status, data_status, verification_status, source_validation_status, source_validation_date, source_validation_note, qa_status, qa_action, source, source_url, source_dataset_version, source_retrieval_date, region_scope_note, rate_status) |>
      datatable(rownames = FALSE, options = list(pageLength = 16, scrollX = TRUE))
  })

  output$country_events <- renderDT({
    if (input$country_iso == "AZE") {
      aze_events() |> select(year_start, year_end, category, event_or_intervention, description, evidence_status, source_citation, source_url, interpretation_limit) |>
        datatable(rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      datatable(data.frame(note = "The intervention dataset currently documents Azerbaijan-specific events; add sourced event records for other countries to compare intervention histories."), options = list(dom = "t"))
    }
  })

  output$qa_summary <- renderDT({
    qa <- cases |> count(qa_status, verification_status, name = "rows") |> arrange(desc(rows))
    datatable(qa, rownames = FALSE, options = list(pageLength = 20, dom = "tip"))
  })

  output$source_register <- renderDT({
    datatable(source_register, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })

  output$qa_rows <- renderDT({
    q <- cases |> filter(is.na(map_value_cases) | qa_status != "needs_source_vintage_validation" | !is.na(qa_action)) |>
      select(country, map_key_iso3, year, source_reported_cases, map_value_cases, qa_status, verification_status, source_validation_status, source_validation_note, qa_action, source, source_url, verification_note, quality_note)
    datatable(q, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  })

  output$geo_qa <- renderDT({
    boundary_codes <- world |> st_drop_geometry() |> distinct(map_key_iso3)
    unmatched <- cases |> distinct(country, map_key_iso3) |>
      filter(is.na(map_key_iso3) | !map_key_iso3 %in% boundary_codes$map_key_iso3)
    datatable(unmatched, rownames = FALSE, options = list(pageLength = 20, dom = "tip"))
  })
}

shinyApp(ui, server)
