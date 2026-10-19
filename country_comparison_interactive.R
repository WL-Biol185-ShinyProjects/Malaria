library(shiny)
library(tidyverse)

long_path <- "Lex/malaria_clean.csv"        # from 01_organize_data.R
wide_path <- "Lex/expansion_csv/malaria_wide.csv"       # from 02_organize_data.R

malaria_long <- read_csv(long_path, show_col_types = FALSE)
malaria_wide <- read_csv(wide_path, show_col_types = FALSE)

country_choices <- sort(unique(malaria_long$country))
my_colors <- c("#f187d0", "#c2168c", "#890f63", "#312c54", "#0c0434")

ui <- fluidPage(
  titlePanel("Compare countries: malaria incidence"),
  
  sidebarLayout(
    sidebarPanel(
      # selectize + maxItems lets the user pick countries
      selectizeInput(
        "countries", "Pick any countries to compare:",
        choices  = country_choices,
        selected = c("Kenya", "Nigeria"),
        multiple = TRUE,
        options  = list(maxItems = 30)
      ),
      checkboxInput(
        "sqrt_scale", "Use square-root color scale (shows small values better)",
        value = FALSE
      ),
      helpText("Incidence per 1,000 people at risk. Gray = no data.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table",   br(), tableOutput("table")),
        tabPanel("Heatmap", br(), plotOutput("heatmap", height = "300px"))
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Table: filter the WIDE data to the chosen countries.
  # req() stops the code quietly until at least one country is picked.
  output$table <- renderTable({
    req(input$countries)
    malaria_wide %>%
      filter(country %in% input$countries) %>%
      select(-code) %>%
      mutate(across(-country, ~ round(.x, 1)))
  }, na = "—", digits = 1)
  
  # Heatmap: filter the LONG data, then draw one colored tile per country-year.
  output$heatmap <- renderPlot({
    req(input$countries)
    
    plot_data <- malaria_long %>%
      filter(country %in% input$countries)
    
    p <- ggplot(plot_data, aes(x = year, y = country, fill = incidence)) +
      geom_tile(color = "#d9d9d9") +
      scale_x_continuous(breaks = seq(2000, 2024, by = 4), expand = c(0, 0)) +
      labs(x = NULL, y = NULL, fill = "Per 1,000\nat risk") +
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank())
    
    if (input$sqrt_scale) {
      p + scale_fill_gradientn(colors = my_colors, na.value = "#d9d9d9",
                               transform = "sqrt")
    } else {
      p + scale_fill_gradientn(colors = my_colors, na.value = "#d9d9d9")
    }
  })
}

shinyApp(ui = ui, server = server)