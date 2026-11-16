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
      
      # Styles for the home page only (all classes start with "home-")
      tags$style(HTML("
        .home-hero {
          background: linear-gradient(135deg, #0c0434 0%, #312c54 55%, #890f63 100%);
          border-radius: 20px;
          padding: clamp(36px, 6vw, 80px) clamp(20px, 4vw, 60px);
          margin: 20px auto 30px auto;
          max-width: 1800px;
          text-align: center;
          box-shadow: 0 10px 30px rgba(0, 0, 0, 0.35);
        }
        .home-hero h1 {
          font-size: clamp(2em, 4.2vw, 3.6em);
          font-weight: 800;
          letter-spacing: 1px;
          margin-bottom: 12px;
          color: #FFADE5;
        }
        .home-hero p {
          font-size: clamp(1.05em, 1.5vw, 1.35em);
          line-height: 1.6;
          max-width: 900px;
          margin: 0 auto;
          color: #f6d5ee;
        }
        .home-kicker {
          text-transform: uppercase;
          letter-spacing: 4px;
          font-size: 0.8em;
          color: #B193CC;
          margin-bottom: 10px;
        }
        .home-section {
          max-width: 1800px;
          margin: 0 auto 30px auto;
        }
        .home-section h3 {
          border-left: 6px solid #c2168c;
          padding-left: 14px;
          margin-bottom: 8px;
        }
        .home-section .home-lead {
          color: #e6c3dc;
          margin-bottom: 18px;
        }
        .home-stats {
          display: flex;
          flex-wrap: wrap;
          gap: 16px;
          margin: 20px 0;
        }
        .home-stat {
          flex: 1 1 200px;
          background: rgba(255, 173, 229, 0.08);
          border: 1px solid rgba(255, 173, 229, 0.25);
          border-radius: 14px;
          padding: 18px 20px;
          text-align: center;
        }
        .home-stat-value {
          font-size: 1.9em;
          font-weight: 800;
          color: #FFADE5;
          line-height: 1.2;
        }
        .home-stat-label {
          font-size: 0.85em;
          text-transform: uppercase;
          letter-spacing: 1px;
          color: #B193CC;
          margin-top: 4px;
        }
        .home-map-frame {
          border: 3px solid #890f63;
          border-radius: 18px;
          padding: 8px;
          background-color: #312c54;
          overflow: hidden;
          box-shadow: 0 8px 24px rgba(0, 0, 0, 0.35);
        }
        .home-map-frame .leaflet-container {
          min-height: 480px;
        }
        .home-note {
          margin-top: 12px;
          font-style: italic;
          font-size: 0.9em;
          color: #B193CC;
        }
        .home-cards {
          display: flex;
          flex-wrap: wrap;
          gap: 18px;
          margin-top: 10px;
        }
        .home-card {
          flex: 1 1 280px;
          background: rgba(255, 173, 229, 0.06);
          border: 1px solid rgba(255, 173, 229, 0.25);
          border-radius: 16px;
          padding: 22px;
          transition: transform 0.2s ease, border-color 0.2s ease;
        }
        .home-card:hover {
          transform: translateY(-4px);
          border-color: #FFADE5;
        }
        .home-card h4 {
          color: #FFADE5;
          margin-bottom: 8px;
        }
        .home-card p {
          color: #e6c3dc;
          min-height: 72px;
        }
        .home-card a {
          font-weight: 700;
          color: #FFADE5;
          text-decoration: none;
        }
        .home-card a:hover {
          text-decoration: underline;
        }
        .home-peek-label {
          font-size: 0.8em;
          text-transform: uppercase;
          letter-spacing: 1px;
          color: #B193CC;
          margin-bottom: 8px;
        }
        .home-peek-big {
          height: 170px;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 2.2em;
          font-weight: 800;
          color: #f187d0;
          margin-bottom: 10px;
        }
        .home-card .shiny-plot-output {
          margin-bottom: 10px;
        }
        .home-links {
          display: flex;
          flex-wrap: wrap;
          gap: 12px;
        }
        .home-links a {
          padding: 10px 20px;
          border: 1px solid rgba(255, 173, 229, 0.4);
          border-radius: 999px;
          color: #FFADE5;
          text-decoration: none;
          font-weight: 600;
          transition: background-color 0.2s ease;
        }
        .home-links a:hover {
          background-color: rgba(255, 173, 229, 0.15);
        }
        .home-footer {
          max-width: 1800px;
          margin: 30px auto 20px auto;
          text-align: center;
          font-size: 0.85em;
          color: #B193CC;
        }
      ")),
      
      # ---- Hero ----
      div(
        class = "home-hero",
        div(class = "home-kicker", "Malaria | Past & Present"),
        h1("Global Malaria Research Project"),
        p(
          "Malaria is preventable and curable, yet it still shapes the ",
          "health of millions of people. Explore where it occurs, how it ",
          "has changed since 2000, and how well treatments are working."
        )
      ),
      
      # ---- Map section ----
      div(
        class = "home-section",
        h3("Global Malaria Incidence, 2000–2024"),
        p(
          class = "home-lead",
          "Incidence per 1,000 people at risk. Press play to watch the ",
          "map change year by year, or drag the slider to pick a year."
        ),
        
        # Slider range is filled in by the server
        sliderInput(
          "year", "Year",
          min = 2000, max = 2024, value = 2006,
          step = 1, sep = "", width = "100%",
          animate = animationOptions(interval = 900, loop = FALSE)
        ),
        
        # Summary numbers for the selected year (filled in by the server)
        div(
          class = "home-stats",
          div(
            class = "home-stat",
            div(class = "home-stat-value", textOutput("stat_countries", inline = TRUE)),
            div(class = "home-stat-label", "Countries reporting")
          ),
          div(
            class = "home-stat",
            div(class = "home-stat-value", textOutput("stat_median", inline = TRUE)),
            div(class = "home-stat-label", "Median incidence per 1,000")
          ),
          div(
            class = "home-stat",
            div(class = "home-stat-value", textOutput("stat_top", inline = TRUE)),
            div(class = "home-stat-label", textOutput("stat_top_label", inline = TRUE))
          ),
          div(
            class = "home-stat",
            div(class = "home-stat-value", textOutput("stat_change", inline = TRUE)),
            div(class = "home-stat-label", "Median change since 2000")
          )
        ),
        
        div(
          class = "home-map-frame",
          leafletOutput("map", height = "calc(100vh - 240px)", width = "100%")
        ),
        
        p(
          class = "home-note",
          "Gray indicates missing data, not necessarily zero incidence."
        )
      ),
      
      # ---- Sneak peeks ----
      div(
        class = "home-section",
        h3("Sneak Peeks"),
        p(
          class = "home-lead",
          "A quick look at what you will find inside. Click any card to jump there."
        ),
        div(
          class = "home-cards",
          
          div(
            class = "home-card",
            div(class = "home-peek-label", textOutput("peek_burden_label", inline = TRUE)),
            plotOutput("peek_burden", height = "170px"),
            h4("Where Malaria Occurs"),
            p("See which countries carry the highest burden and compare any countries side by side."),
            actionLink("go_malaria", "Explore the geography \u2192")
          ),
          
          div(
            class = "home-card",
            div(class = "home-peek-label", "Median country incidence since 2000"),
            plotOutput("peek_trend", height = "170px"),
            h4("Malaria Through Time"),
            p("From ancient descriptions to the discovery of the parasite, and what has changed in this century."),
            actionLink("go_history", "Read the history \u2192")
          ),
          
          div(
            class = "home-card",
            div(class = "home-peek-label", textOutput("peek_who_label", inline = TRUE)),
            plotOutput("peek_who", height = "170px"),
            h4("Treatment and Resistance"),
            p("Explore WHO therapeutic efficacy studies and where antimalarial drugs are failing."),
            actionLink("go_treatment", "Open the dashboard \u2192")
          ),
          
          div(
            class = "home-card",
            div(class = "home-peek-label", "2000 \u2013 2015"),
            div(class = "home-peek-big", "Elimination"),
            h4("Comparative Case Study"),
            p("How malaria was eliminated across the WHO European Region, with a closer look at Azerbaijan."),
            actionLink("go_case", "View the case study \u2192")
          )
        )
      ),
      
      # ---- Learn more (external links) ----
      div(
        class = "home-section",
        h3("Learn More"),
        p(class = "home-lead", "Trusted sources if you want to go deeper."),
        div(
          class = "home-links",
          tags$a(href = "https://www.who.int/news-room/fact-sheets/detail/malaria",
                 target = "_blank", rel = "noopener", "WHO: Malaria fact sheet"),
          tags$a(href = "https://www.who.int/teams/global-malaria-programme",
                 target = "_blank", rel = "noopener", "WHO Global Malaria Programme"),
          tags$a(href = "https://www.cdc.gov/malaria/",
                 target = "_blank", rel = "noopener", "CDC: Malaria"),
          tags$a(href = "https://ourworldindata.org/malaria",
                 target = "_blank", rel = "noopener", "Our World in Data: Malaria")
        )
      ),
      
      div(
        class = "home-footer",
        "Data: Our World in Data (malaria incidence); WHO therapeutic efficacy study database."
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
    
    # ---- Heat map + trends ----
    tabPanel(
      "WHO European Region Heat Map",
      fluidPage(
        br(),
        
        text_section(
          h3("Indigenous Case Extinction Heat Map"),
          p("This map displays estimated malaria cases across the WHO European Region from 2000 to 2015. Use the year slider, or press play, to watch the change over time. Azerbaijan is highlighted, and hovering over a country shows its estimated case count.")
        ),
        
        wide_section(
          sliderInput(
            "eu_year", "Year",
            min = 2000, max = 2015, value = 2000,
            step = 1, sep = "", width = "100%",
            animate = animationOptions(interval = 1000, loop = TRUE)
          ),
          
          div(
            class = "home-map-frame",
            leafletOutput("eu_map", height = "calc(100vh - 300px)", width = "100%")
          ),
          
          p(
            class = "home-note",
            "Dark gray indicates no estimate available for that country and year."
          ),
          
          hr(),
          h4("Malaria Cases Over Time"),
          p("Compare the available global malaria estimates with estimates from the WHO European Region. The two graphs use separate vertical scales so the trend within each group is easier to see."),
          
          fluidRow(
            column(
              width = 6,
              h5("Available Global Estimates"),
              helpText("Combined estimates from the countries included in the global dataset. This is not a complete global total."),
              plotOutput("cs_global_trend", height = "380px")
            ),
            column(
              width = 6,
              h5("WHO European Region"),
              helpText("Combined estimated malaria cases across countries in the WHO European Region."),
              plotOutput("cs_europe_trend", height = "380px")
            )
          ),
          
          br(),
          helpText(
            "About the data: the visualizations use WHO estimated malaria case data. ",
            "The available-country comparison sums estimates for the countries included ",
            "in the global dataset, rather than representing a complete global total."
          )
        )
      )
    ),
    
    # ---- Azerbaijan ----
    tabPanel(
      "Azerbaijan Highlighted",
      fluidPage(
        br(),
        
        text_section(
          h3("Focus: Azerbaijan Case Study"),
          p("Malaria elimination involved sustained public health efforts, including surveillance, prevention, diagnosis, and treatment. Azerbaijan provides an example of progress toward eliminating indigenous malaria transmission.")
        ),
        
        wide_section(
          hr(),
          fluidRow(
            column(
              width = 4,
              sliderInput(
                "az_year", "Year",
                min = 2000, max = 2015, value = 2000,
                step = 1, sep = "", width = "100%"
              ),
              uiOutput("az_summary")
            ),
            column(
              width = 8,
              plotOutput("az_trend", height = "400px")
            )
          ),
          
          hr(),
          h4("Key Milestones"),
          div(
            class = "home-cards",
            div(
              class = "home-card",
              h4("2012"),
              p("Azerbaijan reported zero indigenous cases.")
            ),
            div(
              class = "home-card",
              h4("2015"),
              p("The WHO European Region reached zero indigenous malaria cases.")
            ),
            div(
              class = "home-card",
              h4("2023"),
              p("Azerbaijan was certified malaria-free by WHO.")
            )
          ),
          p(
            class = "home-note",
            "Milestones describe indigenous transmission, not the number of estimated cases."
          )
        )
      )
    ),
    
    # ---- Healthcare resources ----
    tabPanel(
      "Preventative Measures Comparison",
      fluidPage(
        br(),
        
        text_section(
          h3("Preventative Measures & Infrastructure Comparison"),
          p("This graph compares medical doctor availability with estimated malaria cases per 100,000 people across WHO European Region countries. Each point represents one country in the selected year. Azerbaijan is highlighted in pink, and the trend line shows the overall relationship.")
        ),
        
        wide_section(
          hr(),
          fluidRow(
            column(
              width = 3,
              sliderInput(
                "res_year", "Year",
                min = 2000, max = 2015, value = 2000,
                step = 1, sep = "", width = "100%",
                animate = animationOptions(interval = 1500, loop = TRUE)
              ),
              helpText("Only countries with matching malaria, doctor, and population data in the selected year are shown.")
            ),
            column(
              width = 9,
              plotOutput("resource_scatter", height = "460px")
            )
          )
        ),
        
        text_section(
          br(),
          h4("What does the relationship show?"),
          textOutput("resource_description"),
          br(),
          p(
            class = "home-note",
            "Important: doctors per 10,000 people measures healthcare workforce availability, not general healthcare access such as affordability, distance to resources, or how care is distributed. Malaria cases per 100,000 total population is a calculated comparison measure, not the WHO malaria incidence rate per 100,000 people at risk. Further information is necessary to establish a causal relationship."
          )
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
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
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
  # E2. HOME: SUMMARY NUMBERS AND NAVIGATION CARDS
  # ==========================================================
  
  home_year_data <- reactive({
    malaria_countries %>%
      filter(year == input$year)
  })
  
  output$stat_countries <- renderText({
    format(nrow(home_year_data()), big.mark = ",")
  })
  
  output$stat_median <- renderText({
    d <- home_year_data()
    if (nrow(d) == 0) return("\u2014")
    format(round(median(d$incidence), 1), nsmall = 1)
  })
  
  output$stat_top <- renderText({
    d <- home_year_data()
    if (nrow(d) == 0) return("\u2014")
    d$country[which.max(d$incidence)]
  })
  
  output$stat_top_label <- renderText({
    d <- home_year_data()
    if (nrow(d) == 0) return("Highest incidence")
    paste0(
      "Highest incidence (",
      format(round(max(d$incidence), 0), big.mark = ","),
      " per 1,000)"
    )
  })
  
  output$stat_change <- renderText({
    
    base <- malaria_countries %>% filter(year == incidence_years[1])
    now  <- home_year_data()
    
    if (input$year == incidence_years[1] || nrow(base) == 0 || nrow(now) == 0) {
      return("\u2014")
    }
    
    pct <- (median(now$incidence) - median(base$incidence)) /
      median(base$incidence)
    
    paste0(ifelse(pct > 0, "+", ""), round(pct * 100), "%")
  })
  
  # ---- Sneak peek charts on the home page ----
  
  peek_theme <- theme_void(base_size = 12) +
    theme(
      axis.text.y = element_text(color = "#f6d5ee", size = 10, hjust = 1),
      axis.text.x = element_text(color = "#B193CC", size = 9),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  peek_latest_year <- max(malaria_countries$year, na.rm = TRUE)
  
  output$peek_burden_label <- renderText({
    paste("Highest incidence,", peek_latest_year)
  })
  
  output$peek_burden <- renderPlot({
    
    d <- malaria_countries %>%
      filter(year == peek_latest_year) %>%
      slice_max(incidence, n = 5, with_ties = FALSE)
    
    ggplot(d, aes(x = incidence, y = reorder(country, incidence))) +
      geom_col(fill = "#f187d0", width = 0.7) +
      peek_theme
  }, bg = "transparent")
  
  output$peek_trend <- renderPlot({
    
    d <- malaria_countries %>%
      group_by(year) %>%
      summarise(median = median(incidence), .groups = "drop")
    
    ggplot(d, aes(x = year, y = median)) +
      geom_area(fill = "#c2168c", alpha = 0.35) +
      geom_line(color = "#f187d0", linewidth = 1.3) +
      scale_x_continuous(breaks = range(d$year)) +
      peek_theme
  }, bg = "transparent")
  
  output$peek_who_label <- renderText({
    sprintf(
      "%s records from %s countries",
      format(nrow(who), big.mark = ","),
      n_distinct(who$country_name)
    )
  })
  
  output$peek_who <- renderPlot({
    
    d <- who %>%
      filter(is.finite(year_start)) %>%
      count(year_start)
    
    ggplot(d, aes(x = year_start, y = n)) +
      geom_col(fill = "#f187d0", width = 0.8) +
      scale_x_continuous(breaks = range(d$year_start)) +
      peek_theme
  }, bg = "transparent")
  
  # ---- Navigation from the home page ----
  
  observeEvent(input$go_malaria, {
    updateNavbarPage(session, "main_nav", selected = "What is Malaria?")
    updateTabsetPanel(session, "malaria_tabs", selected = "Geographic Distribution")
  })
  
  observeEvent(input$go_history, {
    updateNavbarPage(session, "main_nav", selected = "What is Malaria?")
    updateTabsetPanel(session, "malaria_tabs", selected = "History of Malaria")
  })
  
  observeEvent(input$go_case, {
    updateNavbarPage(session, "main_nav", selected = "WHO European Region Heat Map")
  })
  
  observeEvent(input$go_treatment, {
    updateNavbarPage(session, "main_nav", selected = "WHO Therapeutic Efficacy Dashboard")
  })
  
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
  # CASE STUDY: WHO EUROPEAN REGION, 2000-2015
  # ==========================================================
  
  # Folder holding the three case-study CSV files
  case_study_folder <- "."
  
  cs_error <- NULL
  
  cs <- tryCatch(local({
    
    read_cs <- function(f) {
      path <- file.path(case_study_folder, f)
      if (!file.exists(path)) stop("File not found: ", path)
      read_csv(path, show_col_types = FALSE)
    }
    
    global_raw  <- read_cs("Brianna/estimated_number_of_malaria_cases_2000-2015.csv")
    europe_raw  <- read_cs("Brianna/estimated_number_of_WHO_europe_malaria_cases_2000-2015.csv")
    doctors_raw <- read_cs("Brianna/WHO_global_medical_doctors_per_10,000_(varying_year_ranges).csv")
    
    population_raw <- read.csv(
      "https://ourworldindata.org/grapher/population-unwpp.csv?v=1&csvType=full&useColumnShortNames=true"
    )
    
    # WHO European Region country ISO3 codes
    europe_iso3 <- c(
      "ALB", "AND", "ARM", "AUT", "AZE", "BLR", "BEL",
      "BIH", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
      "FIN", "FRA", "GEO", "DEU", "GRC", "HUN", "ISL",
      "IRL", "ISR", "ITA", "KAZ", "KGZ", "LVA", "LTU",
      "LUX", "MLT", "MCO", "MNE", "NLD", "MKD", "NOR",
      "POL", "PRT", "MDA", "ROU", "RUS", "SMR", "SRB",
      "SVK", "SVN", "ESP", "SWE", "CHE", "TJK", "TUR",
      "TKM", "UKR", "GBR", "UZB"
    )
    
    # Available-country global estimates (one row per country and year)
    global_cases <- global_raw %>%
      transmute(
        iso3 = as.character(SpatialDimValueCode),
        country = as.character(Location),
        year = as.integer(Period),
        cases = as.numeric(FactValueNumeric)
      ) %>%
      filter(year >= 2000, year <= 2015) %>%
      group_by(iso3, country, year) %>%
      summarise(
        cases = if (all(is.na(cases))) NA_real_ else mean(cases, na.rm = TRUE),
        .groups = "drop"
      )
    
    # WHO European Region estimates (one row per country and year)
    europe_cases <- europe_raw %>%
      transmute(
        iso3 = as.character(SpatialDimValueCode),
        country = as.character(country),
        year = as.integer(year),
        cases = as.numeric(FactValueNumeric)
      ) %>%
      filter(iso3 %in% europe_iso3, year >= 2000, year <= 2015) %>%
      group_by(iso3, country, year) %>%
      summarise(
        cases = if (all(is.na(cases))) NA_real_ else mean(cases, na.rm = TRUE),
        .groups = "drop"
      )
    
    global_trend <- global_cases %>%
      group_by(year) %>%
      summarise(
        countries_with_data = sum(!is.na(cases)),
        cases = if (all(is.na(cases))) NA_real_ else sum(cases, na.rm = TRUE),
        .groups = "drop"
      )
    
    europe_trend <- europe_cases %>%
      group_by(year) %>%
      summarise(
        countries_with_data = sum(!is.na(cases)),
        cases = if (all(is.na(cases))) NA_real_ else sum(cases, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Medical doctors per 10,000 people
    doctors_europe <- doctors_raw %>%
      transmute(
        iso3 = as.character(SpatialDimValueCode),
        year = as.integer(Period),
        doctors_per_10000 = as.numeric(FactValueNumeric)
      ) %>%
      filter(iso3 %in% europe_iso3, year >= 2000, year <= 2015) %>%
      group_by(iso3, year) %>%
      summarise(
        doctors_per_10000 = if (all(is.na(doctors_per_10000))) {
          NA_real_
        } else {
          mean(doctors_per_10000, na.rm = TRUE)
        },
        .groups = "drop"
      )
    
    # Population (Our World in Data)
    population_data <- population_raw %>%
      transmute(
        iso3 = as.character(code),
        year = as.integer(year),
        population = as.numeric(
          population__sex_all__age_all__variant_estimates
        )
      ) %>%
      filter(iso3 %in% europe_iso3, year >= 2000, year <= 2015) %>%
      group_by(iso3, year) %>%
      summarise(
        population = if (all(is.na(population))) {
          NA_real_
        } else {
          mean(population, na.rm = TRUE)
        },
        .groups = "drop"
      )
    
    # Cases, doctor availability, and population combined
    resource_comparison <- europe_cases %>%
      inner_join(doctors_europe, by = c("iso3", "year")) %>%
      inner_join(population_data, by = c("iso3", "year")) %>%
      mutate(
        malaria_per_100000 = (cases / population) * 100000,
        is_azerbaijan = iso3 == "AZE"
      ) %>%
      filter(
        !is.na(cases),
        !is.na(doctors_per_10000),
        !is.na(malaria_per_100000),
        !is.na(population),
        population > 0
      )
    
    # Map boundaries with an ISO3 code (reuses the world map loaded above)
    eu_world <- world %>%
      mutate(iso3 = if_else(iso_a3 == "-99", adm0_a3, iso_a3)) %>%
      select(iso3, name)
    
    az_label_point <- eu_world %>%
      filter(iso3 == "AZE") %>%
      sf::st_transform(6933) %>%
      sf::st_point_on_surface() %>%
      sf::st_transform(4326)
    
    case_palette <- colorNumeric(
      palette = c("#21113D", "#63318E", "#B536A0", "#F15BB5", "#FFD166"),
      domain = europe_cases$cases,
      na.color = "#30303A"
    )
    
    list(
      europe_iso3 = europe_iso3,
      europe_cases = europe_cases,
      global_trend = global_trend,
      europe_trend = europe_trend,
      resource_comparison = resource_comparison,
      eu_world = eu_world,
      az_label_point = az_label_point,
      case_palette = case_palette
    )
    
  }), error = function(e) {
    cs_error <<- conditionMessage(e)
    NULL
  })
  
  # Shows a clear message in place of any case-study output if loading failed
  cs_ready <- function() {
    validate(need(
      !is.null(cs),
      paste0("The case study data could not be loaded. ", cs_error)
    ))
  }
  
  # ---- Map ----
  
  make_eu_map_data <- function(selected_year) {
    cs$eu_world %>%
      filter(iso3 %in% cs$europe_iso3) %>%
      left_join(
        cs$europe_cases %>% filter(year == as.integer(selected_year)),
        by = "iso3"
      ) %>%
      mutate(is_azerbaijan = iso3 == "AZE")
  }
  
  add_eu_polygons <- function(map, d) {
    
    map %>%
      addPolygons(
        data = d,
        group = "eu-countries",
        fillColor = ~cs$case_palette(cases),
        fillOpacity = 0.9,
        color = ~ifelse(is_azerbaijan, "#FF3CAC", "#55515F"),
        weight = ~ifelse(is_azerbaijan, 4, 0.8),
        label = ~paste0(
          name,
          ifelse(is_azerbaijan, " - FOCUS COUNTRY", ""),
          ": ",
          ifelse(
            is.na(cases),
            "No estimate available",
            paste0(format(round(cases), big.mark = ","), " estimated cases")
          )
        ),
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#FFD166",
          bringToFront = TRUE
        )
      )
  }
  
  output$eu_map <- renderLeaflet({
    
    cs_ready()
    
    # Draw the starting year here; later years update through the proxy below
    year0 <- isolate(input$eu_year)
    if (is.null(year0)) year0 <- 2000
    
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = 35, lat = 48, zoom = 3) %>%
      add_eu_polygons(make_eu_map_data(year0)) %>%
      addLabelOnlyMarkers(
        data = cs$az_label_point,
        label = ~"AZERBAIJAN",
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "color" = "#FF3CAC",
            "font-weight" = "bold",
            "font-size" = "12px",
            "text-shadow" = "0 0 5px #000000"
          )
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = cs$case_palette,
        values = cs$europe_cases$cases,
        title = "Estimated cases",
        na.label = "No estimate"
      )
  })
  
  observeEvent(input$eu_year, {
    
    req(cs)
    
    leafletProxy("eu_map") %>%
      clearGroup("eu-countries") %>%
      add_eu_polygons(make_eu_map_data(input$eu_year))
    
  }, ignoreInit = TRUE)
  
  # ---- Trend graphs ----
  
  cs_trend_plot <- function(data, graph_title) {
    
    ggplot(data, aes(x = year, y = cases)) +
      geom_line(color = "#c2168c", linewidth = 1.2, na.rm = TRUE) +
      geom_point(color = "#890f63", size = 2.5, na.rm = TRUE) +
      scale_x_continuous(breaks = seq(2000, 2015, by = 3)) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = graph_title,
        x = "Year",
        y = "Estimated malaria cases"
      ) +
      chart_theme
  }
  
  output$cs_global_trend <- renderPlot({
    cs_ready()
    cs_trend_plot(cs$global_trend, "Available global malaria estimates")
  })
  
  output$cs_europe_trend <- renderPlot({
    cs_ready()
    cs_trend_plot(cs$europe_trend, "WHO European Region malaria estimates")
  })
  
  # ---- Azerbaijan ----
  
  output$az_summary <- renderUI({
    
    cs_ready()
    
    az <- cs$europe_cases %>%
      filter(iso3 == "AZE", year == as.integer(input$az_year))
    
    if (nrow(az) == 0 || is.na(az$cases[1])) {
      return(p("No estimate available for this year."))
    }
    
    div(
      class = "home-stats",
      div(
        class = "home-stat",
        div(class = "home-stat-value", format(round(az$cases[1]), big.mark = ",")),
        div(class = "home-stat-label", "Estimated malaria cases")
      ),
      div(
        class = "home-stat",
        div(class = "home-stat-value", input$az_year),
        div(class = "home-stat-label", "Selected year")
      )
    )
  })
  
  output$az_trend <- renderPlot({
    
    cs_ready()
    
    az <- cs$europe_cases %>%
      filter(iso3 == "AZE") %>%
      arrange(year)
    
    validate(need(nrow(az) > 0, "No data available for Azerbaijan."))
    
    cs_trend_plot(az, "Azerbaijan: estimated malaria cases")
  })
  
  # ---- Healthcare resources vs. malaria ----
  
  resource_year_data <- reactive({
    
    cs_ready()
    
    cs$resource_comparison %>%
      filter(year == as.integer(input$res_year))
  })
  
  output$resource_scatter <- renderPlot({
    
    d <- resource_year_data()
    
    validate(need(
      nrow(d) >= 3,
      "Not enough countries have matching data for this year."
    ))
    
    ggplot(d, aes(x = doctors_per_10000, y = malaria_per_100000)) +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        color = "#890f63",
        linewidth = 1.2,
        se = FALSE
      ) +
      geom_point(aes(color = is_azerbaijan), size = 3.5, alpha = 0.85) +
      geom_text(
        data = d %>% filter(is_azerbaijan),
        aes(label = "Azerbaijan"),
        color = "#c2168c",
        nudge_x = 0.1,
        hjust = 0,
        size = 4.5,
        show.legend = FALSE
      ) +
      scale_color_manual(
        values = c("FALSE" = "#B193CC", "TRUE" = "#c2168c"),
        labels = c("FALSE" = "Other countries", "TRUE" = "Azerbaijan"),
        name = NULL
      ) +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = paste("Healthcare resources vs. malaria burden:", input$res_year),
        x = "Medical doctors per 10,000 people",
        y = "Estimated malaria cases per 100,000 people"
      ) +
      chart_theme
  })
  
  output$resource_description <- renderText({
    
    d <- resource_year_data() %>%
      filter(is.finite(doctors_per_10000), is.finite(malaria_per_100000))
    
    if (nrow(d) < 3) {
      return("There is not enough matching country data to calculate a correlation for this year.")
    }
    
    if (
      length(unique(d$doctors_per_10000)) < 2 ||
      length(unique(d$malaria_per_100000)) < 2
    ) {
      return("A correlation cannot be calculated because one variable has too little variation.")
    }
    
    correlation <- cor(
      d$doctors_per_10000,
      d$malaria_per_100000,
      method = "pearson"
    )
    
    strength <- if (abs(correlation) < 0.3) {
      "weak"
    } else if (abs(correlation) < 0.7) {
      "moderate"
    } else {
      "strong"
    }
    
    direction <- if (correlation < 0) {
      "negative"
    } else if (correlation > 0) {
      "positive"
    } else {
      "near-zero"
    }
    
    paste0(
      "For ", input$res_year,
      ", the relationship between the number of medical doctors and malaria cases is ",
      strength, " and ", direction,
      ". Pearson correlation: ", round(correlation, 2),
      ". This describes an association, not evidence that doctor availability causes changes in malaria cases. The comparison includes ",
      nrow(d), " countries with matching data."
    )
  })
  
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