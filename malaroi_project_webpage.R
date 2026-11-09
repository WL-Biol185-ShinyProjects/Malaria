library(shiny)


ui <- navbarPage(
  title = "Malaria | Past & Present"
  theme = #insert colors here!,
    
  tabPanel("Home Page",
           fluidPage(
             h1("global Malaria Research Project"),
             p("this is where you insert the paragraphs and such")
           )), 
  navbarMenu("What is Malaria?", 
             tabPanel("Symptions", 
                      fluidPage(
                        h3("Clinical Symptoms of Malaria")
                        p("this is where you insert the paragraphs and such")
                      )),
             tabPanel("Vecot transmission & Pathology", 
                      fluidPage(
                        h3("Vector Transmission & Pathology"),
                        p("this is where you insert the paragraph and such")
                      )),
             tabPanel("history of Malaria",
                      fluidPage(
                        h3("Historical Context")
                        p("this is where you insert the paragraph and such")
                      )),
             tabPanel("Location Frequency",
                      fluidPage(
                        h3("Global Location Frequency"),
                        p("this is where you insert the paragraph and such")
                      ))
             
  
    
), 
  
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

# Define Server Logic
server <- function(input, output, session) {
  
  # All your interactive plots, maps, and back-end R data calculations will be written here!
  
}
