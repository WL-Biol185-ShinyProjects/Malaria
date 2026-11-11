# WHO therapeutic efficacy studies: Shiny app

library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(leaflet)
library(htmltools)


# 1. Load and clean the data once, when the app starts
who_xlsx  <- "Becca/MTM_THERAPEUTIC_EFFICACY_STUDY_20261012.xlsx"
who_sheet <- "Data"
who_cache <- "Becca/who_clean.rds"   # fast-loading copy of the cleaned data

prepare_who <- function() {
  if (file.exists(who_cache)) return(readRDS(who_cache))
  
  if (!file.exists(who_xlsx)) {
    stop("Data file not found. Put the WHO Excel file at: ", who_xlsx)
  }
  
  d <- read_excel(who_xlsx, sheet = who_sheet) |>
    clean_names()
  
  # Columns that should be numeric (they often import as text)
  num_cols <- intersect(
    c("latitude", "longitude", "year_start",
      "treatment_failure_pp", "treatment_failure_km", "positive_day_3_days"),
    names(d)
  )
  d <- d |>
    mutate(across(all_of(num_cols),
                  ~ suppressWarnings(parse_number(as.character(.x)))))
  
  text_cols <- intersect(
    c("country_name", "site_name", "drug_name", "plasmodium_species"),
    names(d)
  )
  d <- d |>
    mutate(across(all_of(text_cols), ~ str_squish(as.character(.x))))
  
  # Save a fast copy for next time (ignored if the folder is read-only)
  try({
    dir.create(dirname(who_cache), showWarnings = FALSE, recursive = TRUE)
    saveRDS(d, who_cache)
  }, silent = TRUE)
  
  d
}

who <- prepare_who()

required_cols <- c("country_name", "site_name", "drug_name", "plasmodium_species",
                   "year_start", "latitude", "longitude", "treatment_failure_pp")
missing_cols <- setdiff(required_cols, names(who))
if (length(missing_cols) > 0) {
  stop("The data file is missing columns: ", paste(missing_cols, collapse = ", "),
       "\nCheck the column names after clean_names().")
}

# 2. Things computed once
species_choices <- sort(unique(na.omit(who$plasmodium_species)))
default_species <- species_choices[str_detect(str_to_lower(species_choices), "falciparum")]
if (length(default_species) == 0) default_species <- species_choices

drug_choices    <- sort(unique(na.omit(who$drug_name)))
country_choices <- sort(unique(na.omit(who$country_name)))
year_range      <- range(who$year_start, na.rm = TRUE)

measure_choices <- c(
  "Per-protocol treatment failure (%)" = "treatment_failure_pp",
  "Kaplan-Meier treatment failure (%)" = "treatment_failure_km",
  "Day-3 positivity (%)"               = "positive_day_3_days"
)
measure_choices <- measure_choices[measure_choices %in% names(who)]

# Overview tables for the whole dataset
species_counts <- who |> count(plasmodium_species, sort = TRUE)
drug_counts    <- who |> count(drug_name, sort = TRUE) |> slice_head(n = 30)

filter_note <- helpText("Uses the filters on the Resistance and Mutations tab.")


# 3. User interface
ui <- fluidPage(
  titlePanel("Treatment Outcomes"),
  
  tabsetPanel(
    id = "main_tabs",
    
    # ---- Main page: Treatment ----
    tabPanel(
      "Treatment",
      br(),
      fluidRow(
        column(
          width = 9, offset = 2,
          div(
            style = "font-size: 1.05em; line-height: 1.6;",
            h3("Treatment"),
            
            p("Upon diagnosis by a physician, malaria is often treated with antimalarial drugs such as Chloroquine phosphate and Artemisinin-based combination therapies (ACTs)."),
            
            p("Chloroquine phosphate is one of the older drugs often chosen to fight many parasites. However, this has led to drug resistance and reduced efficacy; therefore, it is not commonly used for",
              em("P. falciparum"), "treatment."),
            
            p("ACTs are a combination of drugs that fight malaria in two different ways. This is the preferred treatment with better efficacy. An ACT pairs a fast-acting artemisinin derivative with a slower, longer-lasting partner drug. Artemisinin\u2019s main role is to rapidly kill the main malaria parasite in the red blood cells. The partner drug acts as a longer-lasting drug that also helps prevent drug resistance. Examples of ACTs include artemether-lumefantrine (Coartem) and artesunate-mefloquine."),
            
            p("Other common drugs include Atovaquone-proguanil (Malarone), Quinine sulfate (Qualaquin) with doxycycline (Oracea, Vibramycin, others), Primaquine phosphate."),
            
            p("However, treatment for malaria depends on the parasitic species, the severity of the illness, the patient\u2019s age, and pregnancy status. Overall, according to the CDC, the current treatment for those in the US is artemether-lumefantrine for a duration of 5 days (10 doses) to treat",
              em("Plasmodium falciparum"),
              "malaria, both for the treatment of uncomplicated infections and following IV artesunate for severe infections. WHO recommends an ACT based on therapeutic efficacy studies against local parasites. These include the following combinations:"),
            
            tags$ul(
              tags$li("artemether-lumefantrine (AL)"),
              tags$li("artesunate-amodiaquine"),
              tags$li("artesunate-mefloquine"),
              tags$li("dihydroartemisinin-piperaquine"),
              tags$li("artesunate-sulfadoxine-pyrimethamine"),
              tags$li("artesunate-pyronaridine, which WHO formally recommended in November 2022")
            ),
            
            helpText("This page is general information, not medical advice. Treatment should follow a clinician and current national guidance.")
          )
        )
      )
    ),
    
    # ---- Threats to controlling malaria (second tab) ----
    tabPanel(
      "Threats to Controlling Malaria",
      br(),
      fluidRow(
        column(
          width = 8, offset = 2,
          div(
            style = "font-size: 1.05em; line-height: 1.6;",
            h3("Threats to Controlling Malaria"),
            
            p("According to WHO, there are four major biological threats to controlling malaria: invasive vector species, vector insecticide resistance, antimalarial drug efficacy and resistance, and parasite pfhrp2/3 gene deletions."),
            
            h4("1. Invasive Vector Species"),
            p("The issue with invasive vector species is that they may adapt to changing environments by altering their behavior patterns. Additionally, they may also become insecticide-resistant when exposed to current vector control interventions (WHO)."),
            p("It is recommended by WHO for malaria-endemic countries to engage in entomological surveillance systems to provide the critical information needed in designing and implementing vector control strategies. This includes understanding of vector species\u2019 breeding and resting habitats, behaviors, feeding preferences, and resistance status to prevent the spread (WHO)."),
            
            h4("2. Vector Insecticide Resistance"),
            tags$blockquote("\u201cBetween 2010 and 2020, 88 countries, of which 85 are currently malaria-endemic, reported insecticide resistance data for Anopheles mosquitoes to WHO.\u201d"),
            tags$blockquote("\u201cResistance has been found in vectors from all six WHO regions and to all four classes of insecticide that have been widely used in adult malaria vector control.\u201d"),
            p("To control the spread of mosquitoes, vector control interventions heavily rely on large-scale use of insecticides such as insecticide-treated nets (ITNs) and indoor residual insecticide spraying. Resistance (due to mutation) is a major concern, particularly for pyrethroids, an insecticide class that is commercially available and often utilized (WHO). This has been detected greatly in Western Africa. Resistance to neonicotinoids and the pyrrole insecticide chlorfenapyr (more recently introduced insecticides) has not yet been confirmed and may be an alternative solution until resistance grows; however, this will be more costly."),
            p("WHO recommends enhanced insecticide monitoring and its impact on vector control. It is recommended that all malaria-endemic countries conduct tests once a year on insecticides from all classes in use or planned to be used on all major vector species from the different eco-epidemiological zones. Especially as there are relatively few data in regions other than Africa, such as South-East Asia."),
            
            h4("3. Antimalarial Drug Efficacy and Resistance"),
            p("One of the biggest obstacles in the fight against malaria has been drug resistance. Drug efficacy is monitored through therapeutic efficacy studies (TES). This tracks clinical and parasitological outcomes among patients receiving antimalarial treatment. High treatment failure rates were detected with dihydroartemisinin-piperaquine (DHA-PPQ) in the Western Pacific Region, such as in Cambodia, the Lao People\u2019s Democratic Republic, and Viet Nam."),
            p("These studies are highly recommended as they promote changes in the use of medicine to ensure patients receive effective treatment."),
            p("Artemisinin partial resistance is monitored using an established list of validated and candidate PfKelch13 markers associated with delayed clearance after a treatment containing artemisinin. These PfKelch13 mutations have emerged consistently in Eritrea, Rwanda and Uganda. However, there is an effective partner drug, resulting in a treatment failure rate below 10%. Other areas in which this mutation is highly prevalent are Cambodia, the Lao People\u2019s Democratic Republic and Viet Nam."),
            
            h4("4. Parasite pfhrp2/3 Gene Deletions"),
            p("The malaria parasite may have gene deletions in important markers for detection. This leads to false-negative diagnostic test results (RDTs). The rapid diagnostic test used to diagnose",
              em("Plasmodium falciparum"),
              "heavily relies on RDTs that target an antigen called histidine-rich protein 2 (HRP2), as well as some cross-reactivity with HRP3, a structurally similar parasite protein. It has been found that",
              em("P. falciparum"),
              "parasites lack the pfhrp2 gene that the RDTs target. This was first identified in Peru in 2010.")
          )
        )
      )
    ),
    
    # ---- Project info: filters + interactive map ----
    tabPanel(
      "Resistance and Mutations",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectizeInput("species", "Parasite species",
                         choices = species_choices, selected = default_species,
                         multiple = TRUE),
          selectizeInput("drugs", "Drug", choices = drug_choices, multiple = TRUE,
                         options = list(placeholder = "All drugs")),
          selectizeInput("countries", "Country", choices = country_choices,
                         multiple = TRUE,
                         options = list(placeholder = "All countries")),
          sliderInput("years", "Study start year",
                      min = year_range[1], max = year_range[2],
                      value = year_range, step = 1, sep = ""),
          selectInput("measure", "Outcome shown on the map",
                      choices = measure_choices),
          sliderInput("cap", "Color scale maximum (%)",
                      min = 10, max = 100, value = 50, step = 5),
          hr(),
          textOutput("n_shown"),
          helpText("Records with a missing start year are excluded. ",
                   "Each record is a row in the WHO database, not necessarily a separate study.")
        ),
        mainPanel(
          width = 9,
          leafletOutput("leaf", height = 600)
        )
      )
    ),
    
    tabPanel(
      "Project Drug summary",
      br(),
      filter_note,
      tableOutput("drug_summary"),
      br(),
      downloadButton("dl_summary", "Download CSV")
    ),
    
    tabPanel(
      "Project Dataset overview",
      br(),
      h4("Parasite species (all records)"),
      tableOutput("species_table"),
      h4("Most common drugs (all records)"),
      tableOutput("drug_table")
    )
  )
)


# 4. Server
server <- function(input, output, session) {
  
  # Records matching the sidebar filters
  filtered <- reactive({
    req(input$species)
    d <- who |>
      filter(plasmodium_species %in% input$species,
             !is.na(year_start),
             between(year_start, input$years[1], input$years[2]))
    if (length(input$drugs) > 0)     d <- d |> filter(drug_name %in% input$drugs)
    if (length(input$countries) > 0) d <- d |> filter(country_name %in% input$countries)
    d
  })
  
  # Records with usable coordinates
  geo <- reactive({
    filtered() |>
      filter(!is.na(latitude), !is.na(longitude),
             between(latitude, -90, 90), between(longitude, -180, 180))
  })
  
  # Records with a usable value for the selected outcome
  outcome_data <- reactive({
    m <- input$measure
    geo() |>
      filter(!is.na(.data[[m]]), between(.data[[m]], 0, 100)) |>
      mutate(value = .data[[m]]) |>
      arrange(value)              # lowest first, so the highest draw on top
  })
  
  measure_label <- reactive({
    names(measure_choices)[measure_choices == input$measure]
  })
  
  output$n_shown <- renderText({
    sprintf("%s of %s records match the filters; %s have map coordinates.",
            format(nrow(filtered()), big.mark = ","),
            format(nrow(who), big.mark = ","),
            format(nrow(geo()), big.mark = ","))
  })
  
  # ---- Interactive map ----
  output$leaf <- renderLeaflet({
    d <- outcome_data()
    validate(need(nrow(d) > 0, "No records with coordinates and this outcome match the filters."))
    
    cap <- input$cap
    pal <- colorNumeric(palette = "viridis", domain = c(0, cap), na.color = "#808080")
    lab <- measure_label()
    
    leaflet(d) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~pal(pmin(value, cap)),   # values above the cap get the top color
        fillOpacity = 0.75,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0(
          "<b>Country:</b> ",      htmlEscape(as.character(country_name)),
          "<br><b>Study site:</b> ", htmlEscape(as.character(site_name)),
          "<br><b>Year:</b> ",     htmlEscape(as.character(year_start)),
          "<br><b>Drug:</b> ",     htmlEscape(as.character(drug_name)),
          "<br><b>Species:</b> ",  htmlEscape(as.character(plasmodium_species)),
          "<br><b>", htmlEscape(lab), ":</b> ", round(value, 1), "%"
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = c(0, cap),
        title = lab,
        labFormat = labelFormat(suffix = "%")
      )
  })
  
  # ---- Drug summary ----
  drug_summary <- reactive({
    d <- filtered()
    validate(need(nrow(d) > 0, "No records match the filters."))
    
    d |>
      group_by(drug_name) |>
      summarise(
        records   = n(),
        countries = n_distinct(country_name),
        across(any_of(c("treatment_failure_pp", "treatment_failure_km",
                        "positive_day_3_days")),
               ~ median(.x, na.rm = TRUE), .names = "median_{.col}"),
        .groups = "drop"
      ) |>
      arrange(desc(median_treatment_failure_pp)) |>
      rename(any_of(c(
        "Drug" = "drug_name",
        "Records" = "records",
        "Countries" = "countries",
        "Median failure, per-protocol (%)" = "median_treatment_failure_pp",
        "Median failure, Kaplan-Meier (%)" = "median_treatment_failure_km",
        "Median day-3 positivity (%)" = "median_positive_day_3_days"
      )))
  })
  output$drug_summary <- renderTable(drug_summary(), digits = 1, na = "")
  
  output$dl_summary <- downloadHandler(
    "drug_summary.csv",
    function(file) write_csv(drug_summary(), file)
  )
  
  # ---- Overview tables ----
  output$species_table <- renderTable(
    species_counts |> rename(Species = plasmodium_species, Records = n))
  output$drug_table <- renderTable(
    drug_counts |> rename(Drug = drug_name, Records = n))
}

# Run app
shinyApp(ui = ui, server = server)