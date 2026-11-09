library(shiny)

# Define UI
ui <- navbarPage(
  title = "Malaria | Past & Present",
  theme = bs_theme(
    version = 5, 
    bootswatch = "sandstone",
    bg = "#312c54",        
    fg = "#FFADE5",        
    secondary = "#B193CC", 
    success = "#890f63"    
  ),
  
  # ==========================================
  # HOME PAGE
  # ==========================================
  tabPanel("Home Page",
           fluidPage(
             h1("Global Malaria Research Project"),
             p("Welcome to the homepage. Use the navigation bar above to explore the pathology, history, treatment failures, and comparative case studies."),
             
             hr(), # Adds a clean horizontal divider line under the intro text
             
             fluidRow(
               # Left Column: Your Interactive Map (6 out of 12 grid spaces wide)
               column(width = 6,
                      h3("Interactive Disease Tracking Map"),
                      leafletOutput("malariaMap") 
               ),
               
               # Right Column: Your Graphics / Images (6 out of 12 grid spaces wide)
               column(width = 6,
                      h3("Key Epidemiology Graphics"),
                      imageOutput("homepageGraphic") 
               )
             )
           )
  ), # This cleanly closes the entire Home Page tabPanel
  
  
  
  
  
  
  
  # ==========================================
  # SECTION 1: WHAT IS MALARIA? (DROPDOWN)
  # ==========================================
  navbarMenu("What is Malaria?",
             tabPanel("Symptoms",
                      fluidPage(
                        h3("Clinical Symptoms of Malaria"),
                        p("Content regarding incubation periods, fever cycles, and severe manifestations goes here.")
                      )
             ),
             tabPanel("Vector Transmission & Pathology",
                      fluidPage(
                        h3("Vector Transmission & Pathology"),
                        p("Details on the Anopheles mosquito vector, Plasmodium life cycle, and human host pathology.")
                      )
             ),
             tabPanel("History of Malaria",
                      fluidPage(
                        h3("Historical Context"),
                        p("Timeline of global malaria discovery, historical impacts, and past eradication milestones.")
                      )
             ),
             tabPanel("Location Frequency",
                      fluidPage(
                        h3("Global Location Frequency"),
                        p("Spatial distribution data, endemic zones, and high-burden geographical tracking.")
                      )
             )
  ),
  
  # ==========================================
  # SECTION 2: TREATMENT (DROPDOWN)
  # ==========================================
  navbarMenu("Treatment",
             tabPanel("Treatments",
                      fluidPage(
                        h3("Current Standard Treatments"),
                        p("Overview of standard therapies (e.g., ACTs - Artemisinin-based combination therapies).")
                      )
             ),
             tabPanel("Failed Treatments (4 Reasons)",
                      fluidPage(
                        h3("Failed Treatments"),
                        p("Analysis of historically failed treatment frameworks due to these four major factors:"),
                        tags$ol(
                          tags$li("Inadequate dosage or poor patient compliance."),
                          tags$li("Counterfeit or sub-standard drug quality."),
                          tags$li("Lack of healthcare infrastructure/supply chain breakdowns."),
                          tags$li("Inappropriate self-treatment without proper diagnostics.")
                        )
                      )
             ),
             tabPanel("Resistance & Mutation",
                      fluidPage(
                        h3("Zooming in on Resistance and Mutation"),
                        p("Deep dive into genetic mutations of Plasmodium strains and molecular mechanisms behind drug resistance.")
                      )
             )
  ),
  
  # ==========================================
  # SECTION 3: COMPARATIVE CASE STUDY (DROPDOWN/TABS)
  # ==========================================
  navbarMenu("Comparative Case Study (2000-2015)",
             tabPanel("WHO European Region Heat Map",
                      fluidPage(
                        h3("Indigenous Case Extinction Heat Map"),
                        p("Interactive heatmap or spatial data representing the decline and eradication of malaria cases across the European region between 2000 and 2015.")
                      )
             ),
             tabPanel("Azerbaijan Highlighted",
                      fluidPage(
                        h3("Focus: Azerbaijan Case Study"),
                        p("Detailed analysis of Azerbaijan's timeline towards achieving malaria-free status.")
                      )
             ),
             tabPanel("Preventative Measures Comparison",
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
  
  # ==========================================
  # SECTION 4: ABOUT & RESOURCES
  # ==========================================
  tabPanel("About Us",
           fluidPage(
             h3("About the Research Team"),
             p("Project background, contributor roles, and institutional affiliations.")
           )
  ),
  
  tabPanel("Resources & Works Cited",
           fluidPage(
             h3("Resources"),
             p("Additional helpful external links, databases, and toolkits."),
             hr(),
             h3("Works Cited"),
             p("Academic bibliography, WHO report references, and data sources."),
             hr(),
             h3("Additional Avenues"),
             p("Future directions for malaria research, vaccine updates, and emerging vector control methods.")
           )
  )
)

# Define Server
server <- function(input, output, session) {
  # Server logic, reactive visuals, plots, and maps will go here
}

# Run App
shinyApp(ui, server)
