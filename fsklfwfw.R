# ============================================================
# GLOBAL MALARIA RESEARCH PROJECT
# INTERACTIVE SHINY WEBSITE
#
# Structure:
#   - ui     : page layout and static text ONLY
#   - server : data loading, data prep, filtering, maps, plots,
#              tables, downloads, and filling in input choices
# ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(leaflet)
library(readxl)
library(janitor)
library(htmltools)
library(scales)

# ============================================================
# UI  (layout only: no data, no calculations)
# ============================================================

website_theme <- bs_theme(
  version = 5,
  bootswatch = "sandstone",
  bg = "#312c54",
  fg = "#FFADE5",
  secondary = "#B193CC",
  success = "#890f63"
)

# Layout helpers used by the "What is Malaria?" page
text_section <- function(...) {
  fluidRow(
    column(
      width = 8, offset = 2,
      div(style = "font-size: 1.05em; line-height: 1.6;", ...)
    )
  )
}

wide_section <- function(...) {
  fluidRow(column(width = 10, offset = 1, ...))
}

ui <- navbarPage(
  
  title = "Malaria | Past & Present",
  id = "main_nav",
  theme = website_theme,
  
  # ---------------- HOME ----------------
  
  tabPanel(
    "Home Page",
    fluidPage(
      h1("Global Malaria Research Project"),
      p(
        "Welcome to the homepage. Use the navigation bar above ",
        "to explore the pathology, history, treatment failures, ",
        "and comparative case studies."
      ),
      hr(),
      h3("Global Malaria Incidence, 2000–2024"),
      p(
        "Explore the global distribution of malaria incidence ",
        "per 1,000 people at risk."
      ),
      
      # Slider range is filled in by the server
      sliderInput(
        "year", "Select a year:",
        min = 2000, max = 2024, value = 2006,
        step = 1, sep = "", width = "100%"
      ),
      
      div(
        style = "
          border: 3px solid #890f63;
          border-radius: 15px;
          padding: 8px;
          margin: 20px auto;
          max-width: 1100px;
          background-color: #312c54;
          overflow: hidden;
        ",
        leafletOutput("map", height = "550px", width = "100%")
      ),
      
      p(
        "Gray indicates missing data, not necessarily zero incidence.",
        style = "margin-top: 10px; font-style: italic;"
      )
    )
  ),
  
  # ==========================================================
  # WHAT IS MALARIA?  (one page, inner tabs)
  # ==========================================================
  
  tabPanel(
    "What is Malaria?",
    fluidPage(
      br(),
      
      tabsetPanel(
        id = "malaria_tabs",
        
        # ---- Overview ----
        tabPanel(
          "Overview",
          br(),
          text_section(
            h3("What is Malaria?"),
            p("Malaria is a life-threatening parasitic disease caused by Plasmodium parasites and transmitted primarily through the bites of infected female Anopheles mosquitoes. Although malaria is preventable and curable, it continues to pose a major global health challenge, particularly in tropical and subtropical regions. Malaria is not directly contagious between humans; transmission occurs primarily through infected mosquitoes, although rare cases can occur through blood transfusions, organ transplantation, or congenital transmission."),
            p("Five Plasmodium species are known to cause malaria in humans: ",
              em("P. falciparum"), ", ", em("P. vivax"), ", ", em("P. malariae"), ", ",
              em("P. ovale"), ", and ", em("P. knowlesi"), ". Among these, ",
              em("P. falciparum"), " is responsible for the majority of malaria-related deaths and is especially prevalent in sub-Saharan Africa. ",
              em("P. vivax"), " is the most geographically widespread species outside sub-Saharan Africa and is notable for its ability to cause relapses. The remaining species also contribute to the global burden of malaria, with varying distributions and clinical presentations."),
            p("Although malaria can cause severe illness and death, it is both preventable and curable. Prevention strategies include insecticide-treated mosquito nets, vector control, preventive medications, and malaria vaccines. Early diagnosis and appropriate antimalarial treatment are essential for reducing complications and mortality. Despite these advances, drug resistance, limited healthcare access, and socioeconomic barriers continue to challenge global malaria control efforts.")
          )
        ),
        
        # ---- Symptoms ----
        tabPanel(
          "Symptoms & Clinical Presentations",
          br(),
          text_section(
            h3("Symptoms & Clinical Presentations"),
            p("The symptoms of malaria can vary in severity and are often difficult to distinguish from other common infectious diseases. Initial symptoms typically develop within 10–15 days following infection and may include fever, chills, headache, muscle aches, and fatigue. Because these symptoms are nonspecific, malaria can be difficult to recognize without laboratory testing. Individuals who have recently traveled to or reside in malaria-endemic regions should be evaluated for malaria when compatible symptoms occur."),
            h4("Uncomplicated Malaria"),
            p("Common physical symptoms include:"),
            tags$ul(
              tags$li("Fever and chills"),
              tags$li("Headaches"),
              tags$li("Myalgias (muscle aches)"),
              tags$li("Arthralgias (joint pain)"),
              tags$li("Weakness and fatigue"),
              tags$li("Nausea and vomiting"),
              tags$li("Diarrhea")
            ),
            p("Common clinical findings may include:"),
            tags$ul(
              tags$li("Splenomegaly (enlarged spleen)"),
              tags$li("Anemia"),
              tags$li("Thrombocytopenia (decreased platelet count)")
            ),
            h4("Severe Malaria"),
            p("If left untreated, malaria can progress to severe illness, particularly in infections caused by ",
              em("P. falciparum"), ". Severe malaria may involve:"),
            tags$ul(
              tags$li("Severe anemia"),
              tags$li("Hypoglycemia (low blood glucose)"),
              tags$li("Acute kidney injury"),
              tags$li("Pulmonary edema or respiratory distress"),
              tags$li("Neurological changes, including seizures or impaired consciousness"),
              tags$li("Jaundice"),
              tags$li("Coma")
            ),
            p("Severe malaria is a medical emergency and can be fatal without prompt diagnosis and treatment. Although ",
              em("P. falciparum"), " is most strongly associated with severe malaria, other Plasmodium species can also cause serious complications.")
          )
        ),
        
        # ---- Vector transmission ----
        tabPanel(
          "Vector Transmission & Pathology",
          br(),
          text_section(
            h3("Vector Transmission & Pathology"),
            p("Malaria is primarily transmitted through the bite of an infected female Anopheles mosquito. During a blood meal, the mosquito introduces Plasmodium parasites, known as sporozoites, into the human bloodstream. These parasites travel to the liver, where they invade hepatocytes and undergo replication. Following this liver stage, the parasites are released into the bloodstream as merozoites, which infect red blood cells."),
            p("Within red blood cells, the parasites continue to reproduce and eventually cause the cells to rupture, releasing additional parasites that infect new red blood cells. This cycle of infection and destruction contributes to the characteristic symptoms of malaria, including fever, chills, and anemia. In severe cases, particularly those involving ",
              em("P. falciparum"), ", infected red blood cells can adhere to blood vessel walls and interfere with circulation, contributing to organ dysfunction."),
            p("The malaria parasite also undergoes a sexual reproductive stage within the mosquito, allowing transmission to continue between human hosts. This complex life cycle involves both human and mosquito hosts and is essential to understanding the transmission and pathology of malaria.")
          )
        ),
        
        # ---- History (text + yearly trend) ----
        tabPanel(
          "History of Malaria",
          br(),
          text_section(
            h3("History of Malaria"),
            p("Malaria has affected human populations for thousands of years, with evidence of the disease appearing in ancient civilizations. Early descriptions of malaria-like symptoms were documented in China around 2700 BC and in Greece around 850 BC. Evidence of malaria has also been identified in ancient Egyptian remains, suggesting that the disease was present along the Nile more than 3,000 years ago."),
            p("The term \u201cmalaria\u201d originates from the Italian words ", em("mal"), " and ", em("aria"),
              ", meaning \u201cbad air.\u201d This name reflects the historical belief that the disease was caused by harmful air arising from marshes and wetlands. Although this explanation was eventually disproven, the name remained."),
            p("Significant scientific advancements occurred during the nineteenth century. In 1880, Charles Louis Alphonse Laveran identified the malaria parasite in the blood of infected patients. In 1897, Ronald Ross demonstrated that mosquitoes were responsible for transmitting malaria, establishing the foundation for understanding its transmission cycle."),
            p("Further research into the biology and clinical manifestations of malaria continued throughout the twentieth century. In 1927, Julius Wagner-Jauregg received the Nobel Prize in Physiology or Medicine for his discovery of the therapeutic value of malaria inoculation in the treatment of dementia paralytica, a manifestation of neurosyphilis. This treatment was later replaced by safer and more effective therapies following the development of antibiotics."),
            p("Today, malaria remains a major global health concern despite advances in diagnosis, treatment, and prevention. Continued research focuses on parasite biology, antimalarial drug resistance, vaccine development, and global malaria elimination efforts.")
          ),
          
          wide_section(
            hr(),
            h4("Malaria Since 2000"),
            p("The line shows the median country's incidence each year. The shaded band covers the middle half of countries (25th to 75th percentile). Pick a country to compare it with the global picture."),
            fluidRow(
              column(
                width = 3,
                selectizeInput(
                  "trend_highlight", "Highlight a country (optional)",
                  choices = NULL, multiple = FALSE,
                  options = list(placeholder = "None")
                ),
                sliderInput(
                  "trend_years", "Years",
                  min = 2000, max = 2024, value = c(2000, 2024),
                  step = 1, sep = ""
                ),
                helpText(
                  "Medians are unweighted: every reporting country counts ",
                  "equally regardless of population."
                )
              ),
              column(
                width = 9,
                plotOutput("plot_trend", height = "440px"),
                br(),
                downloadButton("dl_trend_png", "Download PNG")
              )
            )
          )
        ),
        
        # ---- Geographic distribution (text + burden + comparison) ----
        tabPanel(
          "Geographic Distribution",
          br(),
          text_section(
            h3("Geographic Distribution"),
            p("Malaria is primarily found in tropical and subtropical regions where environmental conditions support the survival and reproduction of Anopheles mosquitoes and the development of Plasmodium parasites. The geographic distribution of malaria is influenced by several environmental factors, including temperature, rainfall, humidity, altitude, and the availability of mosquito breeding habitats."),
            p("Today, malaria remains endemic in many countries throughout sub-Saharan Africa, Southeast Asia, the Eastern Mediterranean, the Western Pacific, and parts of Central and South America. The WHO African Region carries the greatest global burden of malaria, accounting for the vast majority of cases and deaths worldwide. However, malaria transmission varies considerably between countries and even within individual regions."),
            p("Historically, malaria was widespread throughout parts of North America, Europe, and Asia. Through improvements in sanitation, mosquito control, healthcare access, and public health interventions, transmission has been eliminated from many regions. Nevertheless, malaria remains a significant concern in endemic areas and can be reintroduced into regions where suitable mosquito vectors remain present."),
            p("Environmental changes, including climate change, can influence the geographic distribution of malaria by altering mosquito habitats, seasonal transmission patterns, and parasite development. However, the relationship between climate change and malaria transmission is complex and is also influenced by socioeconomic conditions, public health infrastructure, and disease prevention strategies.")
          ),
          
          wide_section(
            hr(),
            h4("Where Is Malaria Most Common?"),
            p("The countries with the highest incidence per 1,000 people at risk in the year you choose."),
            fluidRow(
              column(
                width = 3,
                sliderInput(
                  "top_year", "Year",
                  min = 2000, max = 2024, value = 2015, step = 1, sep = ""
                ),
                sliderInput(
                  "top_n", "Number of countries",
                  min = 5, max = 25, value = 10, step = 1
                )
              ),
              column(
                width = 9,
                plotOutput("plot_top", height = "480px"),
                br(),
                downloadButton("dl_top_png", "Download PNG")
              )
            ),
            
            hr(),
            h4("Compare Countries"),
            p("Select two or more countries to compare incidence over time."),
            fluidRow(
              column(
                width = 3,
                selectizeInput(
                  "comp_countries", "Countries",
                  choices = NULL, multiple = TRUE,
                  options = list(placeholder = "Choose countries")
                ),
                sliderInput(
                  "comp_years", "Years",
                  min = 2000, max = 2024, value = c(2000, 2024),
                  step = 1, sep = ""
                ),
                checkboxInput("comp_points", "Show data points", value = TRUE),
                helpText("Countries with no data in the selected years are not drawn.")
              ),
              column(
                width = 9,
                plotOutput("plot_compare", height = "460px"),
                br(),
                downloadButton("dl_compare_png", "Download PNG"),
                downloadButton("dl_compare_csv", "Download CSV"),
                br(), br(),
                h5("First and last values"),
                tableOutput("compare_table")
              )
            )
          )
        )
      )
    )
  ),
  
  # ---------------- COMPARATIVE CASE STUDY ----------------
  
  navbarMenu(
    "Comparative Case Study (2000-2015)",
    
    tabPanel(
      "WHO European Region Heat Map",
      fluidPage(
        h3("Indigenous Case Extinction Heat Map"),
        p(
          "Interactive heatmap or spatial data representing ",
          "the decline and eradication of malaria cases ",
          "across the European region between 2000 and 2015."
        )
      )
    ),
    
    tabPanel(
      "Azerbaijan Highlighted",
      fluidPage(
        h3("Focus: Azerbaijan Case Study"),
        p(
          "Detailed analysis of Azerbaijan's timeline ",
          "towards achieving malaria-free status."
        )
      )
    ),
    
    tabPanel(
      "Preventative Measures Comparison",
      fluidPage(
        h3("Preventative Measures & Infrastructure Comparison"),
        p("Data visualization analyzing healthcare capacity indicators:"),
        tags$ul(
          tags$li("Number of hospitals per population (#)"),
          tags$li("Number of physicians per population (#)")
        )
      )
    )
  ),
  
  # ---------------- TREATMENT ----------------
  
  navbarMenu(
    "Treatment",
    
    tabPanel(
      "WHO Therapeutic Efficacy Dashboard",
      fluidPage(
        h2("WHO Therapeutic Efficacy Studies"),
        p(
          "Explore global antimalarial treatment efficacy, ",
          "treatment failure, drug performance, and study locations."
        ),
        
        sidebarLayout(
          
          # All choices start empty; the server fills them in
          sidebarPanel(
            width = 3,
            
            selectizeInput(
              "species", "Parasite species",
              choices = NULL, multiple = TRUE
            ),
            
            selectizeInput(
              "drugs", "Drug",
              choices = NULL, multiple = TRUE,
              options = list(placeholder = "All drugs")
            ),
            
            selectizeInput(
              "countries", "Country",
              choices = NULL, multiple = TRUE,
              options = list(placeholder = "All countries")
            ),
            
            sliderInput(
              "years", "Study start year",
              min = 2000, max = 2024, value = c(2000, 2024),
              step = 1, sep = ""
            ),
            
            selectInput(
              "measure", "Outcome shown on maps",
              choices = NULL
            ),
            
            sliderInput(
              "cap", "Color scale maximum (%)",
              min = 10, max = 100, value = 50, step = 5
            ),
            
            checkboxInput(
              "crop", "Crop static maps to study region",
              value = TRUE
            ),
            
            hr(),
            textOutput("n_shown"),
            helpText(
              "Records with a missing start year are excluded. ",
              "Each row represents a WHO database record, not ",
              "necessarily a separate study."
            )
          ),
          
          mainPanel(
            width = 9,
            
            tabsetPanel(
              
              tabPanel(
                "WHO Study Locations",
                br(),
                leafletOutput("leaf", height = "600px")
              ),
              
              tabPanel(
                "Treatment Outcome Map",
                br(),
                plotOutput("plot_outcome", height = "560px"),
                br(),
                downloadButton("dl_outcome", "Download PNG")
              ),
              
              tabPanel(
                "Study Locations",
                br(),
                plotOutput("plot_sites", height = "560px"),
                br(),
                downloadButton("dl_sites", "Download PNG")
              ),
              
              tabPanel(
                "Studies by Year",
                br(),
                plotOutput("plot_years", height = "450px"),
                br(),
                downloadButton("dl_years", "Download PNG")
              ),
              
              tabPanel(
                "Drug Summary",
                br(),
                tableOutput("drug_summary"),
                br(),
                downloadButton("dl_summary", "Download CSV")
              ),
              
              tabPanel(
                "Dataset Overview",
                br(),
                h4("Parasite Species (All Records)"),
                tableOutput("species_table"),
                h4("Most Common Drugs (All Records)"),
                tableOutput("drug_table")
              )
            )
          )
        )
      )
    )
  ),
  
  # ---------------- FINAL REMARKS ----------------
  
  tabPanel(
    "Final Remarks",
    fluidPage(
      h3("Resources"),
      p("Additional helpful external links, databases, and toolkits."),
      hr(),
      h3("Works Cited"),
      p("Academic bibliography and data sources."),
      hr(),
      h3("Additional Avenues"),
      p(
        "Future directions for malaria research, vaccine updates, ",
        "and emerging vector control methods."
      )
    )
  ),
  
  # ---------------- ABOUT US ----------------
  
  tabPanel(
    "About Us",
    fluidPage(
      h3("About the Research Team"),
      p(
        "Project background, contributor roles, ",
        "and institutional affiliations."
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ==========================================================
  # A. DATA: MALARIA INCIDENCE
  # ==========================================================
  
  malaria_clean <- local({
    
    malaria <- read_csv(
      "https://ourworldindata.org/grapher/incidence-of-malaria.csv?v=1&csvType=full&useColumnShortNames=false",
      show_col_types = FALSE
    )
    
    entity_col <- names(malaria)[tolower(names(malaria)) == "entity"][1]
    code_col   <- names(malaria)[tolower(names(malaria)) == "code"][1]
    year_col   <- names(malaria)[tolower(names(malaria)) == "year"][1]
    value_col  <- names(malaria)[
      grepl("incidence of malaria", names(malaria), ignore.case = TRUE)
    ][1]
    
    if (any(is.na(c(entity_col, code_col, year_col, value_col)))) {
      stop("A required malaria data column was not found.")
    }
    
    malaria %>%
      transmute(
        country   = .data[[entity_col]],
        code      = .data[[code_col]],
        year      = as.integer(.data[[year_col]]),
        incidence = as.numeric(.data[[value_col]])
      ) %>%
      filter(year >= 2000, year <= 2024)
  })
  
  # Real countries only (drops regional aggregates such as "World")
  malaria_countries <- malaria_clean %>%
    filter(
      !is.na(code),
      nchar(code) == 3,
      !startsWith(code, "OWID"),
      is.finite(incidence)
    )
  
  incidence_countries <- sort(unique(malaria_countries$country))
  incidence_years     <- range(malaria_clean$year, na.rm = TRUE)
  
  # ==========================================================
  # B. DATA: WORLD BOUNDARIES AND COLORS
  # ==========================================================
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  boundary_code <- if ("iso_a3_eh" %in% names(world)) "iso_a3_eh" else "iso_a3"
  
  my_colors <- c("#f187d0", "#c2168c", "#890f63", "#312c54", "#0c0434")
  
  color_domain <- range(malaria_clean$incidence, na.rm = TRUE, finite = TRUE)
  
  pal <- colorNumeric(
    palette = my_colors,
    domain = color_domain,
    na.color = "#d9d9d9"
  )
  
  # ==========================================================
  # C. DATA: WHO THERAPEUTIC EFFICACY
  # ==========================================================
  
  who_xlsx  <- "Becca/MTM_THERAPEUTIC_EFFICACY_STUDY_20261012.xlsx"
  who_sheet <- "Data"
  who_cache <- "Becca/who_clean.rds"
  
  prepare_who <- function() {
    
    if (file.exists(who_cache)) return(readRDS(who_cache))
    
    if (!file.exists(who_xlsx)) {
      stop("WHO Excel file not found: ", who_xlsx)
    }
    
    d <- read_excel(who_xlsx, sheet = who_sheet) %>% clean_names()
    
    num_cols <- intersect(
      c(
        "latitude", "longitude", "year_start",
        "treatment_failure_pp", "treatment_failure_km",
        "positive_day_3_days"
      ),
      names(d)
    )
    
    d <- d %>%
      mutate(
        across(
          all_of(num_cols),
          ~ suppressWarnings(parse_number(as.character(.x)))
        )
      )
    
    text_cols <- intersect(
      c("country_name", "site_name", "drug_name", "plasmodium_species"),
      names(d)
    )
    
    d <- d %>%
      mutate(across(all_of(text_cols), ~ str_squish(as.character(.x))))
    
    try({
      dir.create(dirname(who_cache), showWarnings = FALSE, recursive = TRUE)
      saveRDS(d, who_cache)
    }, silent = TRUE)
    
    d
  }
  
  who <- prepare_who()
  
  required_cols <- c(
    "country_name", "site_name", "drug_name", "plasmodium_species",
    "year_start", "latitude", "longitude", "treatment_failure_pp"
  )
  
  missing_cols <- setdiff(required_cols, names(who))
  if (length(missing_cols) > 0) {
    stop("WHO dataset is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  species_choices <- sort(unique(na.omit(who$plasmodium_species)))
  
  default_species <- species_choices[
    str_detect(str_to_lower(species_choices), "falciparum")
  ]
  if (length(default_species) == 0) default_species <- species_choices
  
  drug_choices    <- sort(unique(na.omit(who$drug_name)))
  country_choices <- sort(unique(na.omit(who$country_name)))
  
  valid_years <- who$year_start[is.finite(who$year_start)]
  if (length(valid_years) == 0) stop("No valid WHO study years were found.")
  year_range <- range(valid_years)
  
  measure_choices <- c(
    "Per-protocol treatment failure (%)" = "treatment_failure_pp",
    "Kaplan-Meier treatment failure (%)" = "treatment_failure_km",
    "Day-3 positivity (%)"               = "positive_day_3_days"
  )
  measure_choices <- measure_choices[measure_choices %in% names(who)]
  
  species_counts <- who %>% count(plasmodium_species, sort = TRUE)
  drug_counts <- who %>% count(drug_name, sort = TRUE) %>% slice_head(n = 30)
  
  crop_x <- c(-90, 165)
  crop_y <- c(-35, 42)
  
  whole_breaks <- function(x) {
    b <- pretty(x, n = 4)
    b[b == round(b) & b >= 1]
  }
  
  map_theme <- theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10),
      legend.position = "right",
      plot.title.position = "plot",
      plot.margin = margin(15, 20, 15, 15)
    )
  
  chart_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.title.position = "plot",
      legend.position = "bottom"
    )
  
  # ==========================================================
  # D. FILL IN INPUTS THE UI LEFT EMPTY
  # ==========================================================
  
  updateSliderInput(session, "year",
                    min = incidence_years[1], max = incidence_years[2], value = 2006)
  
  default_compare <- intersect(
    c("Nigeria", "India", "Ghana", "Brazil"),
    incidence_countries
  )
  
  updateSelectizeInput(session, "comp_countries",
                       choices = incidence_countries,
                       selected = default_compare, server = TRUE)
  
  updateSliderInput(session, "comp_years",
                    min = incidence_years[1], max = incidence_years[2],
                    value = incidence_years)
  
  updateSelectizeInput(session, "trend_highlight",
                       choices = c("None" = "", incidence_countries),
                       selected = "", server = TRUE)
  
  updateSliderInput(session, "trend_years",
                    min = incidence_years[1], max = incidence_years[2],
                    value = incidence_years)
  
  updateSliderInput(session, "top_year",
                    min = incidence_years[1], max = incidence_years[2],
                    value = 2015)
  
  updateSelectizeInput(session, "species",
                       choices = species_choices,
                       selected = default_species, server = TRUE)
  updateSelectizeInput(session, "drugs",
                       choices = drug_choices, server = TRUE)
  updateSelectizeInput(session, "countries",
                       choices = country_choices, server = TRUE)
  updateSliderInput(session, "years",
                    min = year_range[1], max = year_range[2], value = year_range)
  updateSelectInput(session, "measure", choices = measure_choices)
  
  # ==========================================================
  # E. HOME: GLOBAL INCIDENCE MAP
  # ==========================================================
  
  output$map <- renderLeaflet({
    leaflet(world) %>%
      addTiles() %>%
      setView(lng = 15, lat = 15, zoom = 2)
  })
  
  observeEvent(input$year, {
    
    year_data <- malaria_clean %>%
      filter(year == input$year, !is.na(code), code != "") %>%
      select(code, incidence) %>%
      distinct(code, .keep_all = TRUE)
    
    world_year <- world %>%
      left_join(year_data, by = setNames("code", boundary_code))
    
    leafletProxy("map", data = world_year) %>%
      clearGroup("countries") %>%
      removeControl("legend") %>%
      addPolygons(
        group = "countries",
        fillColor = ~pal(incidence),
        fillOpacity = 0.8,
        color = "white",
        weight = 0.5,
        label = ~paste0(
          name, ": ",
          ifelse(is.na(incidence), "No data",
                 paste0(round(incidence, 2), " per 1,000"))
        ),
        highlightOptions = highlightOptions(
          weight = 2, color = "#333333", bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = color_domain,
        title = paste0("Incidence per 1,000 at risk — ", input$year),
        layerId = "legend"
      )
    
  }, ignoreInit = FALSE)
  
  # ==========================================================
  # F. COUNTRY COMPARISON
  # ==========================================================
  
  compare_data <- reactive({
    
    validate(need(length(input$comp_countries) > 0,
                  "Choose at least one country."))
    
    malaria_countries %>%
      filter(
        country %in% input$comp_countries,
        between(year, input$comp_years[1], input$comp_years[2])
      ) %>%
      arrange(country, year)
  })
  
  plot_compare <- reactive({
    
    d <- compare_data()
    
    validate(need(nrow(d) > 0, "No data for these countries and years."))
    
    p <- ggplot(d, aes(x = year, y = incidence, color = country)) +
      geom_line(linewidth = 1.1)
    
    if (isTRUE(input$comp_points)) p <- p + geom_point(size = 2)
    
    p +
      scale_x_continuous(breaks = pretty_breaks()) +
      labs(
        title = "Malaria incidence by country",
        x = "Year",
        y = "Cases per 1,000 people at risk",
        color = NULL,
        caption = "Source: Our World in Data"
      ) +
      chart_theme
  })
  
  output$plot_compare <- renderPlot({
    plot_compare()
  })
  
  output$compare_table <- renderTable({
    
    compare_data() %>%
      group_by(country) %>%
      summarise(
        `First year`  = min(year),
        `First value` = incidence[which.min(year)],
        `Last year`   = max(year),
        `Last value`  = incidence[which.max(year)],
        Change        = `Last value` - `First value`,
        .groups = "drop"
      ) %>%
      rename(Country = country)
  }, digits = 1)
  
  output$dl_compare_png <- downloadHandler(
    filename = function() "country_comparison.png",
    content = function(file) {
      ggsave(file, plot = plot_compare(), width = 10, height = 6,
             units = "in", dpi = 300)
    }
  )
  
  output$dl_compare_csv <- downloadHandler(
    filename = function() "country_comparison.csv",
    content = function(file) write_csv(compare_data(), file)
  )
  
  # ==========================================================
  # G. YEARLY TREND
  # ==========================================================
  
  trend_summary <- reactive({
    
    malaria_countries %>%
      filter(between(year, input$trend_years[1], input$trend_years[2])) %>%
      group_by(year) %>%
      summarise(
        median = median(incidence),
        q25 = quantile(incidence, 0.25),
        q75 = quantile(incidence, 0.75),
        countries = n(),
        .groups = "drop"
      )
  })
  
  plot_trend <- reactive({
    
    s <- trend_summary()
    
    validate(need(nrow(s) > 0, "No data for these years."))
    
    hl <- input$trend_highlight
    has_hl <- !is.null(hl) && nzchar(hl)
    
    p <- ggplot(s, aes(x = year)) +
      geom_ribbon(aes(ymin = q25, ymax = q75),
                  fill = "#c2168c", alpha = 0.25) +
      geom_line(aes(y = median, color = "Median country"), linewidth = 1.3)
    
    line_colors <- c("Median country" = "#312c54")
    
    if (has_hl) {
      h <- malaria_countries %>%
        filter(country == hl,
               between(year, input$trend_years[1], input$trend_years[2]))
      
      if (nrow(h) > 0) {
        p <- p + geom_line(data = h,
                           aes(y = incidence, color = hl),
                           linewidth = 1.3)
        line_colors <- c(line_colors, setNames("#f187d0", hl))
      }
    }
    
    p +
      scale_color_manual(values = line_colors) +
      scale_x_continuous(breaks = pretty_breaks()) +
      labs(
        title = "Malaria incidence over time",
        subtitle = "Median across reporting countries, with the middle 50% shaded",
        x = "Year",
        y = "Cases per 1,000 people at risk",
        color = NULL,
        caption = "Source: Our World in Data"
      ) +
      chart_theme
  })
  
  output$plot_trend <- renderPlot({
    plot_trend()
  })
  
  output$dl_trend_png <- downloadHandler(
    filename = function() "yearly_trend.png",
    content = function(file) {
      ggsave(file, plot = plot_trend(), width = 10, height = 6,
             units = "in", dpi = 300)
    }
  )
  
  # ==========================================================
  # H. HIGHEST BURDEN
  # ==========================================================
  
  plot_top <- reactive({
    
    d <- malaria_countries %>%
      filter(year == input$top_year) %>%
      slice_max(incidence, n = input$top_n, with_ties = FALSE)
    
    validate(need(nrow(d) > 0, "No data for this year."))
    
    ggplot(d, aes(x = incidence, y = reorder(country, incidence))) +
      geom_col(fill = "#890f63") +
      geom_text(aes(label = round(incidence, 0)), hjust = -0.15, size = 3.8) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(
        title = paste("Highest malaria incidence,", input$top_year),
        x = "Cases per 1,000 people at risk",
        y = NULL,
        caption = "Source: Our World in Data"
      ) +
      chart_theme
  })
  
  output$plot_top <- renderPlot({
    plot_top()
  })
  
  output$dl_top_png <- downloadHandler(
    filename = function() "highest_burden.png",
    content = function(file) {
      ggsave(file, plot = plot_top(), width = 10, height = 6,
             units = "in", dpi = 300)
    }
  )
  
  # ==========================================================
  # I. WHO DASHBOARD: FILTERED DATA
  # ==========================================================
  
  filtered <- reactive({
    
    req(input$species, input$years)
    
    d <- who %>%
      filter(
        plasmodium_species %in% input$species,
        !is.na(year_start),
        between(year_start, input$years[1], input$years[2])
      )
    
    if (length(input$drugs) > 0) {
      d <- d %>% filter(drug_name %in% input$drugs)
    }
    if (length(input$countries) > 0) {
      d <- d %>% filter(country_name %in% input$countries)
    }
    
    d
  })
  
  geo <- reactive({
    filtered() %>%
      filter(
        !is.na(latitude), !is.na(longitude),
        between(latitude, -90, 90),
        between(longitude, -180, 180)
      )
  })
  
  outcome_data <- reactive({
    
    req(input$measure)
    m <- input$measure
    
    geo() %>%
      filter(!is.na(.data[[m]]), between(.data[[m]], 0, 100)) %>%
      mutate(value = .data[[m]]) %>%
      arrange(value)
  })
  
  measure_label <- reactive({
    req(input$measure)
    names(measure_choices)[measure_choices == input$measure]
  })
  
  output$n_shown <- renderText({
    sprintf(
      "%s of %s records match the filters; %s have map coordinates.",
      format(nrow(filtered()), big.mark = ","),
      format(nrow(who), big.mark = ","),
      format(nrow(geo()), big.mark = ",")
    )
  })
  
  # ==========================================================
  # J. WHO DASHBOARD: INTERACTIVE MAP
  # ==========================================================
  
  output$leaf <- renderLeaflet({
    
    d <- outcome_data()
    
    validate(need(
      nrow(d) > 0,
      "No records with coordinates and this outcome match the filters."
    ))
    
    cap <- input$cap
    
    who_pal <- colorNumeric(
      palette = "viridis",
      domain = c(0, cap),
      na.color = "#808080"
    )
    
    lab <- measure_label()
    
    leaflet(d) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~who_pal(pmin(value, cap)),
        fillOpacity = 0.75,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0(
          "<b>Country:</b> ", htmlEscape(as.character(country_name)),
          "<br><b>Study site:</b> ", htmlEscape(as.character(site_name)),
          "<br><b>Year:</b> ", htmlEscape(as.character(year_start)),
          "<br><b>Drug:</b> ", htmlEscape(as.character(drug_name)),
          "<br><b>Species:</b> ", htmlEscape(as.character(plasmodium_species)),
          "<br><b>", htmlEscape(lab), ":</b> ", round(value, 1), "%"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = who_pal,
        values = c(0, cap),
        title = lab,
        labFormat = labelFormat(suffix = "%")
      )
  })
  
  # ==========================================================
  # K. WHO DASHBOARD: STATIC MAPS AND CHARTS
  # ==========================================================
  
  plot_outcome <- reactive({
    
    d <- outcome_data()
    validate(need(nrow(d) > 0, "No records match the selected filters."))
    
    cap <- input$cap
    lab <- measure_label()
    
    ggplot() +
      geom_sf(data = world, fill = "grey95", color = "grey75", linewidth = 0.2) +
      geom_point(
        data = d,
        aes(x = longitude, y = latitude, color = value),
        size = 2, alpha = 0.75
      ) +
      scale_color_viridis_c(
        option = "C", limits = c(0, cap),
        oob = scales::squish, name = lab
      ) +
      coord_sf(
        xlim = if (input$crop) crop_x,
        ylim = if (input$crop) crop_y,
        expand = FALSE,
        default_crs = st_crs(4326)
      ) +
      labs(
        title = paste("Global distribution:", tolower(lab)),
        subtitle = paste0(
          "WHO therapeutic efficacy studies; values above ",
          cap, "% shown in the top color"
        ),
        caption = "Source: WHO therapeutic efficacy study database.",
        x = NULL, y = NULL
      ) +
      map_theme
  })
  
  output$plot_outcome <- renderPlot({ plot_outcome() })
  
  plot_sites <- reactive({
    
    d <- geo() %>%
      count(longitude, latitude, name = "number_of_records") %>%
      arrange(desc(number_of_records))
    
    validate(need(nrow(d) > 0, "No records with coordinates match the filters."))
    
    ggplot() +
      geom_sf(data = world, fill = "grey95", color = "grey75", linewidth = 0.2) +
      geom_point(
        data = d,
        aes(x = longitude, y = latitude, size = number_of_records),
        color = "firebrick", alpha = 0.55
      ) +
      scale_size_area(
        max_size = 7, breaks = whole_breaks, name = "Records at coordinate"
      ) +
      coord_sf(
        xlim = if (input$crop) crop_x,
        ylim = if (input$crop) crop_y,
        expand = FALSE,
        default_crs = st_crs(4326)
      ) +
      labs(
        title = "Global distribution of malaria efficacy studies",
        subtitle = "Circle size represents the number of records at each coordinate",
        caption = "Source: WHO therapeutic efficacy study database",
        x = NULL, y = NULL
      ) +
      map_theme
  })
  
  output$plot_sites <- renderPlot({ plot_sites() })
  
  plot_years <- reactive({
    
    d <- filtered() %>% count(year_start)
    validate(need(nrow(d) > 0, "No records match the filters."))
    
    ggplot(d, aes(x = year_start, y = n)) +
      geom_col(fill = "#890f63") +
      labs(
        title = "WHO therapeutic efficacy studies by year",
        x = "Study start year",
        y = "Number of records"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$plot_years <- renderPlot({ plot_years() })
  
  # ==========================================================
  # L. WHO DASHBOARD: TABLES
  # ==========================================================
  
  drug_summary <- reactive({
    
    d <- filtered()
    validate(need(nrow(d) > 0, "No records match the filters."))
    
    result <- d %>%
      group_by(drug_name) %>%
      summarise(
        records = n(),
        countries = n_distinct(country_name),
        across(
          any_of(c("treatment_failure_pp", "treatment_failure_km",
                   "positive_day_3_days")),
          ~ if (all(is.na(.x))) NA_real_ else median(.x, na.rm = TRUE),
          .names = "median_{.col}"
        ),
        .groups = "drop"
      )
    
    if ("median_treatment_failure_pp" %in% names(result)) {
      result <- result %>% arrange(desc(median_treatment_failure_pp))
    }
    
    result
  })
  
  output$drug_summary <- renderTable({ drug_summary() }, digits = 1, na = "")
  
  output$species_table <- renderTable({
    species_counts %>% rename(Species = plasmodium_species, Records = n)
  })
  
  output$drug_table <- renderTable({
    drug_counts %>% rename(Drug = drug_name, Records = n)
  })
  
  # ==========================================================
  # M. WHO DASHBOARD: DOWNLOADS
  # ==========================================================
  
  save_png <- function(p, w, h) {
    function(file) {
      ggsave(file, plot = p(), width = w, height = h, units = "in", dpi = 300)
    }
  }
  
  output$dl_outcome <- downloadHandler(
    filename = function() "treatment_outcome_map.png",
    content  = save_png(plot_outcome, 12, 6)
  )
  output$dl_sites <- downloadHandler(
    filename = function() "study_locations_map.png",
    content  = save_png(plot_sites, 12, 6)
  )
  output$dl_years <- downloadHandler(
    filename = function() "studies_by_year.png",
    content  = save_png(plot_years, 9, 5)
  )
  output$dl_summary <- downloadHandler(
    filename = function() "drug_summary.csv",
    content  = function(file) write_csv(drug_summary(), file)
  )
}

# ============================================================
# LAUNCH
# ============================================================

shinyApp(ui = ui, server = server)