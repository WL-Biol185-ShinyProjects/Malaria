# ============================================================
# GLOBAL MALARIA RESEARCH PROJECT
# INTERACTIVE SHINY WEBSITE
#
# Structure:
#   - ui     : page layout and static text ONLY
#   - server : data loading, data prep, filtering, maps, plots,
#              tables, downloads, and filling in input choices
#
# Images are expected in the "www" folder next to this file.
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

# Layout helpers
text_section <- function(...) {
  fluidRow(
    column(
      width = 10, offset = 1,
      div(style = "font-size: 1.05em; line-height: 1.6;", ...)
    )
  )
}

wide_section <- function(...) {
  fluidRow(column(width = 10, offset = 1, ...))
}

# ------------------------------------------------------------
# Helpers for the "What is Malaria?" page (layout only)
# ------------------------------------------------------------

# Italicize parasite and mosquito names inside plain text
italicize <- function(x) {
  
  pattern <- paste0(
    "(P\\. ovale (curtisi|wallikeri)",
    "|P\\. (falciparum|vivax|malariae|ovale|knowlesi)",
    "|Plasmodium( (falciparum|vivax|malariae|ovale|knowlesi))?",
    "|Anopheles( gambiae)?",
    "|Artemisia annua)"
  )
  
  m <- gregexpr(pattern, x, perl = TRUE)
  hits <- regmatches(x, m)[[1]]
  rest <- regmatches(x, m, invert = TRUE)[[1]]
  
  out <- list()
  for (i in seq_along(rest)) {
    out <- c(out, list(rest[i]))
    if (i <= length(hits)) out <- c(out, list(em(hits[i])))
  }
  out
}

# One or more paragraphs from plain text
paras <- function(...) {
  tagList(lapply(c(...), function(t) p(italicize(t))))
}

# A bold label followed by text, as a list item
mal_li <- function(label, text) {
  tags$li(strong(paste0(label, ":")), " ", italicize(text))
}

mal_page <- function(...) div(class = "mal-page", ...)

mal_hero <- function(kicker, title, text) {
  div(
    class = "mal-hero",
    div(class = "mal-kicker", kicker),
    h2(title),
    p(text)
  )
}

mal_heading <- function(text, lead = NULL) {
  tagList(
    h3(class = "mal-h", text),
    if (!is.null(lead)) p(class = "mal-lead", lead)
  )
}

mal_source <- function(...) p(class = "mal-source", ...)

# Clickable highlight cards.
#   jump : id of a section further down the page (opens it and scrolls)
#   tab  : title of another inner tab to switch to first
#   nav  : title of a top-level page or menu item to open
mal_cards <- function(...) div(class = "mal-cards", ...)

mal_card <- function(icon, title, text,
                     jump = NULL, tab = NULL, nav = NULL,
                     hint = "Read more \u2193") {
  div(
    class = "mal-card",
    `data-jump` = jump,
    `data-tab`  = tab,
    `data-nav`  = nav,
    div(class = "mal-card-icon", icon),
    h4(title),
    p(text),
    span(class = "mal-card-link", hint)
  )
}

# "Keep exploring" row of cards for the bottom of a page
mal_next <- function(...) {
  tagList(
    mal_heading("Keep Exploring", "Where to go next."),
    mal_cards(...)
  )
}

# A section that a card (or back link) can scroll to
jump_section <- function(id, ..., class = NULL) {
  div(id = id, class = paste(c("jump-target", class), collapse = " "), ...)
}

# Small "back to" link that scrolls to a section
back_link <- function(target, label = "\u2191 Back to the top") {
  span(class = "back-link", `data-jump` = target, label)
}

# A boxed figure: dark frame, soft mat behind the image, caption underneath
fig_box <- function(src, caption = NULL, label = NULL, tag = NULL, alt = NULL) {
  div(
    class = "fig-box",
    if (!is.null(tag)) div(class = "fig-tag", tag),
    div(
      class = "fig-mat",
      img(src = utils::URLencode(src, reserved = FALSE), alt = if (is.null(alt)) "" else alt)
    ),
    if (!is.null(caption)) {
      p(class = "fig-caption", if (!is.null(label)) strong(label), " ", caption)
    }
  )
}

# Expandable detail section (native HTML details/summary)
mal_detail <- function(id, title, ..., open = FALSE) {
  tags$details(
    id = id,
    class = "mal-detail",
    open = if (open) NA else NULL,
    tags$summary(title),
    div(class = "mal-detail-body", ...)
  )
}

# Big-number statistics
mal_stats <- function(...) div(class = "mal-stats", ...)

mal_stat <- function(value, label) {
  div(
    class = "mal-stat",
    div(class = "mal-stat-value", value),
    div(class = "mal-stat-label", label)
  )
}

# Vertical timeline
mal_timeline <- function(...) div(class = "mal-timeline", ...)

mal_event <- function(year, title, text, jump = NULL) {
  div(
    class = "mal-event",
    `data-jump` = jump,
    div(class = "mal-event-year", year),
    div(
      class = "mal-event-body",
      h4(title),
      p(text),
      if (!is.null(jump)) span(class = "mal-card-link", "Read more \u2193")
    )
  )
}

# Numbered steps (life cycle, poverty cycle)
mal_steps <- function(...) div(class = "mal-steps", ...)

mal_step <- function(number, title, text, jump = NULL) {
  div(
    class = "mal-step",
    `data-jump` = jump,
    div(class = "mal-step-number", number),
    h4(title),
    p(text)
  )
}

# Styles shared by every page (figures, tables, plots, back links)
shared_css <- "
  .jump-target { scroll-margin-top: 90px; }
  .cs-jump     { scroll-margin-top: 150px; }

  .back-link {
    display: inline-block;
    margin: 8px 0 18px 0;
    cursor: pointer;
    font-weight: 700;
    font-size: 0.9em;
    color: #FFADE5;
  }
  .back-link:hover { text-decoration: underline; }

  /* Boxed figures */
  .fig-box {
    max-width: 860px;
    margin: 22px 0;
    padding: 16px;
    background: rgba(255, 173, 229, 0.06);
    border: 1px solid rgba(255, 173, 229, 0.28);
    border-radius: 18px;
    box-shadow: 0 6px 18px rgba(0, 0, 0, 0.25);
  }
  .fig-mat {
    background: #f4eefa;
    border-radius: 12px;
    padding: 10px;
    line-height: 0;
  }
  .fig-mat img {
    display: block;
    width: 100%;
    height: auto;
    margin: 0 auto;
    border-radius: 8px;
  }
  .fig-caption {
    margin: 12px 4px 0 4px;
    font-size: 0.9em;
    line-height: 1.55;
    color: #e6c3dc;
  }
  .fig-caption strong { color: #FFADE5; }
  .fig-tag {
    margin-bottom: 8px;
    font-size: 0.85em;
    color: #B193CC;
  }

  /* Every ggplot gets a rounded, bordered frame */
  .shiny-plot-output {
    border-radius: 14px;
    overflow: hidden;
    border: 1px solid rgba(255, 173, 229, 0.25);
  }

  /* Dataset overview panels and tables */
  .ds-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(380px, 1fr));
    gap: 20px;
    margin: 14px 0 10px 0;
  }
  .ds-panel {
    background: rgba(255, 173, 229, 0.05);
    border: 1px solid rgba(255, 173, 229, 0.25);
    border-radius: 16px;
    padding: 18px 20px;
    margin-bottom: 16px;
    overflow-x: auto;
  }
  .ds-panel h4 {
    color: #FFADE5;
    margin-top: 0;
    font-size: 1.1em;
  }
  .ds-scroll {
    max-height: 440px;
    overflow-y: auto;
  }
  .ds-panel .table {
    --bs-table-color: #f6d5ee;
    --bs-table-bg: transparent;
    --bs-table-striped-color: #f6d5ee;
    --bs-table-striped-bg: rgba(255, 173, 229, 0.07);
    --bs-table-hover-color: #ffffff;
    --bs-table-hover-bg: rgba(255, 173, 229, 0.14);
    --bs-table-border-color: rgba(255, 173, 229, 0.2);
    margin-bottom: 0;
  }
  .ds-panel .table th {
    color: #FFADE5;
    border-bottom: 2px solid rgba(255, 173, 229, 0.4);
  }
"


reference_entry <- function(citation, url, note = NULL) {
  div(
    class = "reference-entry",
    p(class = "reference-citation", citation),
    tags$a(href = url, target = "_blank", rel = "noopener noreferrer",
           "Open source \u2197"),
    if (!is.null(note)) p(class = "reference-note", note)
  )
}

reference_group <- function(id, title, ..., open = FALSE) {
  mal_detail(id, title, ..., open = open)
}

ui <- navbarPage(
  
  title = "Malaria | Past & Present",
  id = "main_nav",
  theme = website_theme,
  header = tags$style(HTML(shared_css)),
  
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
        .home-stat-link {
          display: block;
          text-decoration: none !important;
          color: inherit;
        }
        .home-stat-clickable {
          transition: transform 0.2s ease, border-color 0.2s ease;
        }
        .home-stat-clickable:hover {
          transform: translateY(-3px);
          border-color: #FFADE5;
        }
        .home-stat-hint {
          margin-top: 8px;
          font-size: 0.8em;
          font-weight: 700;
          color: #FFADE5;
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
          background: #1b1738;
        }
        #leaf.leaflet-container {
          background: #9fb3c8;
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
        h3("Global Malaria Incidence, 2000\u20132024"),
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
            class = "home-stat home-stat-clickable",
            actionLink(
              "go_top",
              label = tagList(
                div(class = "home-stat-value", textOutput("stat_top", inline = TRUE)),
                div(class = "home-stat-label", textOutput("stat_top_label", inline = TRUE)),
                div(class = "home-stat-hint", "See it on the chart \u2192")
              ),
              class = "home-stat-link"
            )
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
            div(class = "home-peek-label", "Estimated cases, WHO European Region, 2000\u20132015"),
            plotOutput("peek_case", height = "170px"),
            h4("Eliminating Malaria in Europe"),
            p("Watch estimated cases across the WHO European Region fall between 2000 and 2015, with a closer look at Azerbaijan."),
            actionLink("go_case", "View the case study \u2192")
          )
        )
      ),
      
      # ---- Explore the basics (jump cards) ----
      div(
        class = "home-section",
        h3("Explore the Basics"),
        p(class = "home-lead", "Short on time? Jump straight to a topic."),
        mal_cards(
          mal_card("\U0001F321\uFE0F", "Symptoms & diagnosis",
                   "How malaria feels, how severe disease develops, and how it is tested for.",
                   nav = "What is Malaria?", tab = "Symptoms & Clinical Presentations",
                   hint = "Open \u2192"),
          mal_card("\U0001F99F", "How it spreads",
                   "The five stages of the parasite life cycle and what they do to the body.",
                   nav = "What is Malaria?", tab = "Vector Transmission & Pathology",
                   hint = "Open \u2192"),
          mal_card("\U0001F465", "Who is most affected",
                   "Children, pregnant women, and the link between malaria and poverty.",
                   nav = "What is Malaria?", tab = "Who Is Most Affected",
                   hint = "Open \u2192"),
          mal_card("\U0001F326\uFE0F", "Climate & resistance",
                   "Why a changing climate and evolving resistance make control harder.",
                   nav = "What is Malaria?", tab = "Climate & Resistance",
                   hint = "Open \u2192"),
          mal_card("\U0001F48A", "Treatment options",
                   "Which drugs are used, and how WHO and the CDC recommend using them.",
                   nav = "Treatment", tab = "Treatment Options",
                   hint = "Open \u2192"),
          mal_card("\U0001F5C2\uFE0F", "Dataset overview",
                   "What is inside the WHO therapeutic efficacy database.",
                   nav = "Treatment", tab = "Dataset overview",
                   hint = "Open \u2192")
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
      
      # Styles for this page only (all classes start with "mal-")
      tags$style(HTML("
        .mal-page {
          width: 100%;
          max-width: 1500px;
          margin: 0 auto;
          padding-bottom: 40px;
        }
        .mal-hero {
          background: linear-gradient(135deg, #0c0434 0%, #312c54 55%, #890f63 100%);
          border-radius: 18px;
          padding: clamp(24px, 4vw, 48px) clamp(20px, 4vw, 48px);
          margin: 10px 0 28px 0;
          box-shadow: 0 8px 24px rgba(0, 0, 0, 0.3);
        }
        .mal-hero h2 {
          color: #FFADE5;
          font-weight: 800;
          font-size: clamp(1.6em, 3vw, 2.4em);
          margin: 0 0 10px 0;
        }
        .mal-hero p {
          color: #f6d5ee;
          font-size: 1.1em;
          line-height: 1.6;
          max-width: 820px;
          margin: 0;
        }
        .mal-kicker {
          text-transform: uppercase;
          letter-spacing: 4px;
          font-size: 0.75em;
          color: #B193CC;
          margin-bottom: 8px;
        }
        .mal-h {
          border-left: 6px solid #c2168c;
          padding-left: 14px;
          margin: 34px 0 8px 0;
        }
        .mal-lead {
          color: #e6c3dc;
          margin-bottom: 16px;
        }
        .mal-source {
          font-size: 0.85em;
          font-style: italic;
          color: #B193CC;
          margin: 12px 0 0 0;
        }
        .mal-cards {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 16px;
          margin: 14px 0 10px 0;
          width: 100%;
        }
        @media (min-width: 900px) {
          .mal-cards { grid-template-columns: repeat(3, minmax(0, 1fr)); }
          .mal-cards:has(> .mal-card:nth-child(4):last-child) { grid-template-columns: repeat(4, minmax(0, 1fr)); }
        }
        @media (min-width: 1200px) {
          .mal-cards:has(> .mal-card:nth-child(5):last-child) { grid-template-columns: repeat(5, minmax(0, 1fr)); }
        }
        @media (max-width: 599px) {
          .mal-cards { grid-template-columns: minmax(0, 1fr); }
        }
        .mal-card {
          background: rgba(255, 173, 229, 0.07);
          border: 1px solid rgba(255, 173, 229, 0.25);
          border-radius: 16px;
          padding: 20px;
          cursor: pointer;
          transition: transform 0.2s ease, border-color 0.2s ease, background 0.2s ease;
        }
        .mal-card:hover {
          transform: translateY(-4px);
          border-color: #FFADE5;
          background: rgba(255, 173, 229, 0.13);
        }
        .mal-card-icon {
          font-size: 2em;
          line-height: 1;
          margin-bottom: 8px;
        }
        .mal-card h4 {
          color: #FFADE5;
          margin: 0 0 8px 0;
          font-size: 1.15em;
        }
        .mal-card p {
          color: #e6c3dc;
          margin-bottom: 10px;
          line-height: 1.5;
        }
        .mal-card-link {
          font-weight: 700;
          font-size: 0.9em;
          color: #FFADE5;
        }
        .mal-stats {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
          gap: 14px;
          margin: 14px 0 6px 0;
        }
        .mal-stat {
          background: rgba(255, 173, 229, 0.08);
          border: 1px solid rgba(255, 173, 229, 0.25);
          border-radius: 14px;
          padding: 18px;
          text-align: center;
        }
        .mal-stat-value {
          font-size: 2em;
          font-weight: 800;
          color: #FFADE5;
          line-height: 1.2;
        }
        .mal-stat-label {
          font-size: 0.85em;
          color: #B193CC;
          margin-top: 6px;
          line-height: 1.4;
        }
        .mal-detail {
          background: rgba(255, 173, 229, 0.05);
          border: 1px solid rgba(255, 173, 229, 0.2);
          border-radius: 14px;
          margin-bottom: 12px;
          scroll-margin-top: 80px;
        }
        .mal-detail[open] {
          border-color: rgba(255, 173, 229, 0.55);
        }
        .mal-detail summary {
          cursor: pointer;
          padding: 14px 20px;
          font-weight: 700;
          font-size: 1.05em;
          color: #FFADE5;
          list-style: none;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        .mal-detail summary::-webkit-details-marker {
          display: none;
        }
        .mal-detail summary::after {
          content: '+';
          font-size: 1.4em;
          color: #B193CC;
        }
        .mal-detail[open] summary::after {
          content: '\\2212';
        }
        .mal-detail-body {
          padding: 0 22px 16px 22px;
          max-width: 920px;
          line-height: 1.7;
        }
        .mal-detail-body ul {
          padding-left: 22px;
        }
        .mal-detail-body li {
          margin-bottom: 8px;
        }
        .mal-flash {
          animation: mal-flash 1.6s ease;
        }
        @keyframes mal-flash {
          0%   { box-shadow: 0 0 0 0 rgba(255, 173, 229, 0.9); }
          100% { box-shadow: 0 0 0 18px rgba(255, 173, 229, 0); }
        }
        .mal-steps {
          display: flex;
          flex-wrap: wrap;
          gap: 14px;
          margin: 14px 0;
        }
        .mal-step {
          flex: 1 1 180px;
          position: relative;
          background: rgba(255, 173, 229, 0.07);
          border: 1px solid rgba(255, 173, 229, 0.25);
          border-radius: 16px;
          padding: 20px 16px 16px 16px;
          text-align: center;
          cursor: pointer;
          transition: transform 0.2s ease, border-color 0.2s ease;
        }
        .mal-step:hover {
          transform: translateY(-4px);
          border-color: #FFADE5;
        }
        .mal-step-number {
          width: 42px;
          height: 42px;
          line-height: 42px;
          border-radius: 50%;
          background: #c2168c;
          color: white;
          font-weight: 800;
          margin: 0 auto 10px auto;
        }
        .mal-step h4 {
          color: #FFADE5;
          font-size: 1.05em;
          margin: 0 0 6px 0;
        }
        .mal-step p {
          color: #e6c3dc;
          font-size: 0.92em;
          margin: 0;
        }
        .mal-timeline {
          border-left: 3px solid #c2168c;
          margin: 20px 0 20px 18px;
          padding-left: 0;
        }
        .mal-event {
          display: flex;
          gap: 18px;
          position: relative;
          padding: 4px 0 22px 26px;
          cursor: pointer;
        }
        .mal-event::before {
          content: '';
          position: absolute;
          left: -10px;
          top: 10px;
          width: 17px;
          height: 17px;
          border-radius: 50%;
          background: #FFADE5;
          border: 3px solid #312c54;
        }
        .mal-event-year {
          flex: 0 0 110px;
          font-size: 1.3em;
          font-weight: 800;
          color: #f187d0;
        }
        .mal-event-body {
          flex: 1 1 auto;
          padding: 10px 16px;
          border-radius: 12px;
          transition: background 0.2s ease;
        }
        .mal-event:hover .mal-event-body {
          background: rgba(255, 173, 229, 0.1);
        }
        .mal-event h4 {
          color: #FFADE5;
          margin: 0 0 6px 0;
        }
        .mal-event p {
          color: #e6c3dc;
          margin-bottom: 8px;
        }
        .mal-callout {
          background: linear-gradient(135deg, rgba(137, 15, 99, 0.45), rgba(49, 44, 84, 0.6));
          border: 1px solid #c2168c;
          border-radius: 18px;
          padding: 24px 28px;
          margin: 28px 0;
        }
        .mal-callout h3 {
          color: #FFADE5;
          margin-top: 0;
        }
        .mal-note {
          font-size: 0.9em;
          font-style: italic;
          color: #B193CC;
        }
      ")),
      
      # Click handling: open a section and scroll to it, switch to another
      # inner tab first, or open another page of the site.
      # Works for any element with data-jump, data-tab or data-nav.
      tags$script(HTML("
        document.addEventListener('click', function(e) {
          var t = e.target.closest('[data-jump], [data-tab], [data-nav]');
          if (!t) return;
          e.preventDefault();
          
          var jump = t.getAttribute('data-jump');
          var tab  = t.getAttribute('data-tab');
          var nav  = t.getAttribute('data-nav');
          
          function openLink(value) {
            var links = document.querySelectorAll('a[data-value]');
            for (var i = 0; i < links.length; i++) {
              if (links[i].getAttribute('data-value') === value) {
                links[i].click();
                return;
              }
            }
          }
          
          function scrollToSection() {
            if (!jump) return;
            var el = document.getElementById(jump);
            if (!el) return;
            if (el.tagName === 'DETAILS') el.open = true;
            el.scrollIntoView({ behavior: 'smooth', block: 'start' });
            el.classList.remove('mal-flash');
            void el.offsetWidth;
            el.classList.add('mal-flash');
          }
          
          if (nav) openLink(nav);
          if (tab) {
            openLink(tab);
            setTimeout(scrollToSection, 350);
          } else {
            scrollToSection();
          }
        });
      ")),
      
      br(),
      
      tabsetPanel(
        id = "malaria_tabs",
        
        # ======================================================
        # OVERVIEW
        # ======================================================
        tabPanel(
          "Overview",
          br(),
          mal_page(
            
            mal_hero(
              "Start here",
              "What is Malaria?",
              "A preventable and curable disease caused by Plasmodium parasites and spread by the bite of infected female Anopheles mosquitoes."
            ),
            
            mal_heading(
              "Malaria by the Numbers",
              "Global estimates for 2024 from the WHO World Malaria Report 2025."
            ),
            mal_stats(
              mal_stat("282 million", "malaria cases worldwide in 2024"),
              mal_stat("610,000", "malaria deaths worldwide in 2024"),
              mal_stat("~80", "countries where malaria remained endemic"),
              mal_stat("94%", "of cases were in the WHO African Region (95% of deaths)"),
              mal_stat("75%", "of malaria deaths in the African Region were children under five"),
              mal_stat("2.3 billion", "cases and 14 million deaths averted worldwide, 2000\u20132024")
            ),
            mal_source("Source: WHO, World Malaria Report 2025. Malaria is preventable and curable, yet it continues to disproportionately affect communities facing poverty and barriers to healthcare."),
            
            mal_heading("The Big Picture", "Click a card to read more."),
            mal_cards(
              mal_card("\U0001F99F", "Spread by mosquitoes, not people",
                       "Malaria cannot be caught by touching, coughing, or sharing food. Rare exceptions exist.",
                       jump = "ov-contagious"),
              mal_card("\U0001F9EC", "Five parasite species",
                       "From the deadliest to the ones that hide in the liver. Meet them below.",
                       jump = "ov-species-cards"),
              mal_card("\U0001F476", "Who it hits hardest",
                       "Young children, pregnant women, and people without prior exposure.",
                       tab = "Who Is Most Affected", hint = "Open \u2192"),
              mal_card("\U0001F99F", "How it spreads",
                       "Five stages of the parasite life cycle, from mosquito bite to the next host.",
                       tab = "Vector Transmission & Pathology", hint = "Open \u2192")
            ),
            
            mal_heading("The Basics"),
            mal_detail(
              "ov-what", "What is malaria?", open = TRUE,
              paras(
                "Malaria is a potentially life-threatening infectious disease caused by parasites belonging to the genus Plasmodium. The disease is primarily transmitted to humans through the bites of infected female Anopheles mosquitoes. Although malaria is preventable and curable, it remains one of the world's most significant infectious diseases, particularly in tropical and subtropical regions where environmental conditions support mosquito populations and parasite development.",
                "Unlike bacterial or viral infections, malaria is caused by a eukaryotic parasite that undergoes a complex life cycle involving both human and mosquito hosts. After entering the human body, the parasite initially infects liver cells before entering the bloodstream and invading red blood cells. The destruction of infected red blood cells, combined with the body's immune response, produces many of the characteristic symptoms of malaria, including fever, chills, fatigue, and anemia."
              )
            ),
            mal_detail(
              "ov-contagious", "Is malaria contagious?",
              paras(
                "Malaria is not ordinarily contagious through direct person-to-person contact. Individuals cannot contract malaria through touching, coughing, sneezing, or sharing food with someone who is infected. However, rare cases of transmission can occur through contaminated blood transfusions, organ transplantation, shared needles, or transmission from a pregnant individual to their developing fetus."
              )
            ),
            mal_detail(
              "ov-who", "Who is affected, and why does it persist?",
              paras(
                "Malaria disproportionately affects populations living in areas with limited healthcare infrastructure, inadequate access to preventive measures, and high exposure to mosquito vectors. Young children, pregnant women, and individuals without previous exposure to malaria are particularly vulnerable to severe disease.",
                "Despite major advancements in prevention, diagnosis, and treatment, malaria continues to cause hundreds of millions of infections and hundreds of thousands of deaths worldwide each year. The persistence of malaria reflects not only the biological complexity of the parasite but also broader challenges involving poverty, healthcare inequality, climate, environmental conditions, and public health infrastructure."
              )
            ),
            mal_source("Sources: CDC, About Malaria; CDC DPDx, Malaria; WHO, Malaria Fact Sheet."),
            
            mal_heading(
              "The Five Species of Malaria Parasites",
              "Five Plasmodium species are traditionally recognized as major causes of human malaria. Click a species to learn more."
            ),
            div(
              id = "ov-species-cards",
              mal_cards(
                mal_card("\U0001F480", "Plasmodium falciparum",
                         "Causes most malaria deaths and dominates in sub-Saharan Africa.",
                         jump = "sp-falciparum"),
                mal_card("\U0001F4A4", "Plasmodium vivax",
                         "Widespread outside Africa and can hide in the liver, then relapse.",
                         jump = "sp-vivax"),
                mal_card("\u23F1\uFE0F", "Plasmodium malariae",
                         "Slow, long-lasting infections with a 72-hour fever cycle.",
                         jump = "sp-malariae"),
                mal_card("\U0001F30D", "Plasmodium ovale",
                         "Found mainly in Africa and the Western Pacific. Can also relapse.",
                         jump = "sp-ovale"),
                mal_card("\U0001F412", "Plasmodium knowlesi",
                         "Spreads from macaque monkeys and replicates every 24 hours.",
                         jump = "sp-knowlesi")
              )
            ),
            mal_detail(
              "sp-falciparum", "Plasmodium falciparum",
              paras("P. falciparum is responsible for the majority of malaria-related deaths worldwide and is especially prevalent in sub-Saharan Africa. This species is particularly dangerous because it can infect red blood cells of different ages, allowing high parasite densities to develop. Infected red blood cells may also adhere to the walls of small blood vessels, interfering with circulation and contributing to severe complications such as cerebral malaria, kidney injury, and respiratory distress. Without prompt treatment, severe P. falciparum infection can progress rapidly and become fatal.")
            ),
            mal_detail(
              "sp-vivax", "Plasmodium vivax",
              paras("P. vivax is widely distributed outside sub-Saharan Africa, particularly in parts of Asia, the Americas, and the Western Pacific. Unlike P. falciparum, P. vivax preferentially infects younger red blood cells known as reticulocytes. One of its most important characteristics is its ability to form dormant liver-stage parasites called hypnozoites. These dormant parasites can reactivate weeks, months, or occasionally years after the initial infection, producing recurrent episodes of malaria without a new mosquito bite. Although P. vivax is often associated with less severe illness than P. falciparum, it can still cause serious complications and death.")
            ),
            mal_detail(
              "sp-malariae", "Plasmodium malariae",
              paras("P. malariae occurs in several regions worldwide and is generally associated with lower parasite densities and a slower progression of infection. It has a characteristic 72-hour red blood cell replication cycle, which can produce fever episodes approximately every three days. In some individuals, the parasite can persist at low levels for many years without causing obvious symptoms. Chronic infections have also been associated with certain kidney complications.")
            ),
            mal_detail(
              "sp-ovale", "Plasmodium ovale",
              paras("P. ovale is found primarily in parts of sub-Saharan Africa and some regions of the Western Pacific. Like P. vivax, it can form dormant hypnozoites in the liver, allowing malaria to recur after the initial infection has resolved. Two closely related species, P. ovale curtisi and P. ovale wallikeri, are now recognized within this group.")
            ),
            mal_detail(
              "sp-knowlesi", "Plasmodium knowlesi",
              paras("P. knowlesi is a zoonotic malaria parasite found primarily in Southeast Asia. Its natural hosts include macaque monkeys, and transmission to humans occurs through infected mosquitoes. This parasite has a rapid, approximately 24-hour red blood cell replication cycle, which can allow parasite concentrations to increase quickly. Although some infections are relatively mild, P. knowlesi can also cause severe and potentially fatal malaria.")
            ),
            
            mal_next(
              mal_card("\U0001F321\uFE0F", "Symptoms & diagnosis",
                       "What malaria feels like and how it is detected.",
                       tab = "Symptoms & Clinical Presentations", hint = "Open \u2192"),
              mal_card("\U0001F99F", "The parasite life cycle",
                       "Follow the parasite from mosquito to human and back.",
                       tab = "Vector Transmission & Pathology", hint = "Open \u2192"),
              mal_card("\U0001F30D", "Where malaria occurs",
                       "The geography and global burden of the disease.",
                       tab = "Geographic Distribution", hint = "Open \u2192")
            )
          )
        ),
        
        # ======================================================
        # SYMPTOMS & DIAGNOSIS
        # ======================================================
        tabPanel(
          "Symptoms & Clinical Presentations",
          br(),
          mal_page(
            
            mal_hero(
              "Recognizing the disease",
              "Symptoms & Clinical Presentations",
              "Malaria symptoms are often nonspecific and look like other infections, so diagnosis cannot rely on symptoms alone."
            ),
            
            mal_heading("Key Points", "Click a card to jump to the details."),
            mal_cards(
              mal_card("\U0001F321\uFE0F", "Fever and chills",
                       "Linked to parasites bursting out of red blood cells.",
                       jump = "sy-fever"),
              mal_card("\U0001FA78", "Anemia",
                       "Red blood cells are destroyed, removed, and under-produced.",
                       jump = "sy-anemia"),
              mal_card("\U0001F6A8", "Severe malaria is an emergency",
                       "Brain, kidney, lung, and circulation complications can be fatal.",
                       jump = "sy-severe"),
              mal_card("\U0001F52C", "How is it diagnosed?",
                       "Microscopy, rapid tests, and PCR each have strengths and limits.",
                       jump = "dx-micro")
            ),
            
            mal_heading("Symptoms"),
            mal_detail(
              "sy-overview", "When do symptoms appear?", open = TRUE,
              paras(
                "The symptoms of malaria vary depending on the infecting Plasmodium species, parasite density, the individual's immune response, and whether the person has previously been exposed to malaria. In many cases, the initial symptoms are nonspecific and resemble those of other infectious diseases, making malaria difficult to recognize without laboratory testing.",
                "Symptoms commonly begin approximately 7\u201330 days after infection, although the incubation period varies by parasite species and may be longer in certain circumstances. Infections involving dormant liver-stage parasites can produce symptoms months or even years after the original exposure."
              )
            ),
            mal_detail(
              "sy-uncomplicated", "Uncomplicated malaria",
              paras(
                "Uncomplicated malaria refers to symptomatic infection without evidence of severe organ dysfunction or other criteria for severe malaria.",
                "Common symptoms include fever, chills, sweating, headache, muscle aches, joint pain, fatigue, weakness, nausea, vomiting, abdominal discomfort, and occasionally diarrhea.",
                "Some individuals experience episodes of fever followed by chills and profuse sweating. These episodes are associated with the rupture of infected red blood cells and the release of parasite material into the bloodstream, which stimulates the immune system to produce inflammatory signaling molecules.",
                "In certain infections, fever episodes may eventually follow a recognizable pattern corresponding to the parasite's replication cycle. For example, P. vivax and P. ovale are traditionally associated with approximately 48-hour fever cycles, while P. malariae is associated with a 72-hour cycle. However, these patterns are often absent or irregular, particularly early in infection, and should not be relied upon for diagnosis.",
                "Additional clinical findings may include anemia, thrombocytopenia, enlargement of the spleen, and mild jaundice."
              )
            ),
            mal_detail(
              "sy-fever", "Why does malaria cause fever?",
              paras(
                "Fever is one of the most recognizable symptoms of malaria and is closely associated with the parasite's development within red blood cells.",
                "During the blood stage of infection, malaria parasites multiply inside red blood cells until the cells rupture, releasing newly formed parasites and parasite-derived molecules into circulation. These substances activate immune cells and stimulate the production of inflammatory cytokines, including tumor necrosis factor-alpha, interleukin-1, and interleukin-6.",
                "These inflammatory signals affect the hypothalamus, which regulates body temperature, contributing to fever and chills. Although this immune response helps the body recognize and respond to infection, excessive inflammation can contribute to tissue injury and complications."
              )
            ),
            mal_detail(
              "sy-anemia", "Why does malaria cause anemia?",
              paras(
                "Anemia occurs when the number of circulating red blood cells or the concentration of hemoglobin falls below normal levels.",
                "Malaria contributes to anemia through several mechanisms. First, infected red blood cells are destroyed when parasites complete their replication cycle. Second, the spleen removes both infected and uninfected red blood cells from circulation. Third, inflammatory responses can interfere with the production of new red blood cells in the bone marrow.",
                "In severe infections, these combined mechanisms can produce substantial reductions in hemoglobin concentration, impairing the body's ability to transport oxygen to tissues.",
                "Severe malarial anemia is particularly important among young children living in highly endemic regions, where repeated infections and other contributing conditions can increase vulnerability."
              )
            ),
            mal_detail(
              "sy-severe", "Severe malaria",
              paras(
                "Severe malaria occurs when infection produces life-threatening complications or significant organ dysfunction. Although it is most commonly associated with P. falciparum, other species, including P. vivax and P. knowlesi, can also cause severe disease.",
                "Complications include:"
              ),
              tags$ul(
                mal_li("Cerebral malaria", "A serious neurological complication characterized by impaired consciousness, coma, and sometimes seizures. Cerebral malaria is particularly associated with P. falciparum infection and involves sequestration of infected red blood cells in the brain's microvasculature, endothelial dysfunction, and inflammation."),
                mal_li("Severe anemia", "Extensive destruction and removal of red blood cells can result in dangerously low hemoglobin concentrations, reducing oxygen delivery throughout the body."),
                mal_li("Acute kidney injury", "Severe malaria can impair kidney function through inflammation, reduced tissue perfusion, and microvascular abnormalities. Some patients may require dialysis."),
                mal_li("Respiratory distress", "Severe infection can produce metabolic acidosis, acute respiratory distress syndrome, or pulmonary edema, interfering with normal breathing and oxygen exchange."),
                mal_li("Hypoglycemia", "Low blood glucose may occur in severe malaria, particularly in children and pregnant patients, and can also be associated with certain antimalarial treatments."),
                mal_li("Jaundice", "Increased destruction of red blood cells and liver dysfunction can cause bilirubin accumulation, producing yellow discoloration of the skin and eyes."),
                mal_li("Shock and circulatory dysfunction", "Severe infections may produce circulatory instability and inadequate blood flow to vital organs.")
              ),
              paras("Severe malaria is a medical emergency requiring immediate hospitalization and appropriate antimalarial treatment. Early diagnosis and treatment substantially reduce the risk of complications and death.")
            ),
            mal_source("Sources: CDC, Clinical Features of Malaria; CDC, Evaluation and Diagnosis; WHO, Guidelines for Malaria."),
            
            mal_heading(
              "Diagnosis",
              "Early and accurate diagnosis is essential for preventing severe malaria and reducing transmission."
            ),
            mal_detail(
              "dx-intro", "Why symptoms are not enough",
              paras(
                "Because malaria symptoms frequently resemble those of other infectious diseases, diagnosis cannot reliably be made from symptoms alone.",
                "Healthcare providers must consider an individual's symptoms, recent travel history, potential exposure to infected mosquitoes, and laboratory testing results. Several diagnostic methods are available, each with different advantages and limitations."
              )
            ),
            mal_detail(
              "dx-micro", "Microscopic examination of blood smears",
              paras(
                "Microscopy is one of the most widely established methods for diagnosing malaria. A blood sample is collected from the patient, placed onto a glass slide, stained, and examined under a microscope for the presence of Plasmodium parasites.",
                "Two types of blood smears are commonly used. Thick blood smears allow laboratory personnel to examine a relatively large volume of blood, making them particularly useful for detecting malaria parasites when parasite concentrations are low. Thin blood smears preserve the appearance of individual red blood cells, allowing laboratory personnel to identify parasite species, examine developmental stages, and estimate the percentage of infected red blood cells.",
                "Microscopy is especially valuable because identifying the parasite species and measuring parasite density can help guide treatment decisions. However, microscopy requires trained personnel, appropriate laboratory equipment, and quality control. In remote or resource-limited settings, these requirements may delay diagnosis."
              )
            ),
            mal_detail(
              "dx-rdt", "Rapid diagnostic tests",
              paras(
                "Rapid diagnostic tests, commonly called RDTs, detect specific malaria parasite antigens in a small blood sample. These tests often use a disposable cassette or test strip and can provide results within approximately 15\u201320 minutes. They are particularly valuable in areas where laboratory microscopy is unavailable or difficult to access.",
                "However, they have important limitations. Some tests cannot reliably distinguish between different malaria parasite species, and test sensitivity varies depending on parasite density and the specific antigens being detected.",
                "Certain P. falciparum parasites have genetic deletions affecting the histidine-rich protein 2 or 3 antigens, potentially causing false-negative results in tests that depend on those proteins. Because of these limitations, negative rapid diagnostic tests do not always exclude malaria, particularly when clinical suspicion remains high."
              )
            ),
            mal_detail(
              "dx-pcr", "Polymerase chain reaction (PCR)",
              paras(
                "Polymerase chain reaction, or PCR, is a molecular diagnostic method that detects genetic material from malaria parasites. PCR is generally more sensitive than routine microscopy and can help identify infections with very low parasite concentrations. It is also useful for distinguishing between Plasmodium species and identifying mixed infections involving more than one species.",
                "Despite its sensitivity, PCR may require specialized equipment, trained laboratory personnel, and longer processing times. Consequently, conventional PCR is often used for confirmation rather than as the sole method for urgent diagnosis."
              )
            ),
            mal_detail(
              "dx-early", "Why early diagnosis matters",
              paras(
                "Delayed diagnosis allows malaria parasites to continue multiplying within red blood cells, increasing the risk of severe disease and complications. This is particularly dangerous for P. falciparum infections, which can progress rapidly.",
                "Early diagnosis also supports malaria control programs by identifying infected individuals who may contribute to continued transmission. In regions working toward malaria elimination, accurate detection of both symptomatic and asymptomatic infections is important for understanding where transmission persists."
              )
            ),
            mal_source("Sources: CDC, Malaria Diagnostic Tests; CDC, Evaluation and Diagnosis of Malaria."),
            
            mal_next(
              mal_card("\U0001F99F", "The parasite life cycle",
                       "Why the blood stage is what makes people sick.",
                       tab = "Vector Transmission & Pathology", hint = "Open \u2192"),
              mal_card("\U0001F465", "Who is most affected",
                       "Why some people face a much higher risk of severe disease.",
                       tab = "Who Is Most Affected", hint = "Open \u2192"),
              mal_card("\U0001F48A", "Treatment options",
                       "Which drugs are used once malaria is diagnosed.",
                       nav = "Treatment", tab = "Treatment Options", hint = "Open \u2192")
            )
          )
        ),
        
        # ======================================================
        # VECTOR TRANSMISSION & PATHOLOGY
        # ======================================================
        tabPanel(
          "Vector Transmission & Pathology",
          br(),
          mal_page(
            
            mal_hero(
              "How it spreads and what it does",
              "Vector Transmission & Pathology",
              "The parasite moves between mosquito and human through five distinct stages, and the blood stage is what makes people sick."
            ),
            
            mal_heading(
              "The Parasite Life Cycle",
              "Click a stage to read what happens."
            ),
            mal_steps(
              mal_step("1", "Mosquito bite", "An infected mosquito injects sporozoites.", jump = "lc-1"),
              mal_step("2", "Liver infection", "Parasites multiply silently in liver cells.", jump = "lc-2"),
              mal_step("3", "Red blood cells", "Repeated invasion and rupture causes symptoms.", jump = "lc-3"),
              mal_step("4", "Gametocytes", "Sexual forms circulate, ready for the next mosquito.", jump = "lc-4"),
              mal_step("5", "Inside the mosquito", "Sexual reproduction produces new sporozoites.", jump = "lc-5")
            ),
            mal_detail(
              "lc-intro", "Why the life cycle matters", open = TRUE,
              paras(
                "Malaria transmission involves a complex biological relationship between Plasmodium parasites, female Anopheles mosquitoes, and human hosts. The parasite undergoes distinct developmental stages in both the mosquito and human body, allowing it to reproduce, survive, and spread between individuals.",
                "Understanding the malaria life cycle is essential for explaining how the disease develops, why symptoms occur, and how different prevention and treatment strategies work."
              )
            ),
            mal_detail(
              "lc-1", "Stage 1: Transmission through the mosquito bite",
              paras(
                "Malaria transmission begins when an infected female Anopheles mosquito takes a blood meal from a human. Female mosquitoes require blood meals to support egg production. During feeding, an infected mosquito introduces saliva containing the infective parasite stage, called sporozoites, into the human body.",
                "The sporozoites enter circulation and travel toward the liver, where the next stage of parasite development occurs."
              )
            ),
            mal_detail(
              "lc-2", "Stage 2: Liver infection",
              paras(
                "After reaching the liver, sporozoites invade hepatocytes, the primary functional cells of the liver. Inside these cells, the parasites undergo asexual replication, producing large numbers of daughter parasites called merozoites. This process is known as exoerythrocytic schizogony.",
                "During this initial liver stage, individuals generally do not experience the characteristic symptoms of malaria because the parasites have not yet begun their repeated cycles of red blood cell infection and destruction. Eventually, infected liver cells release merozoites into the bloodstream.",
                "In P. vivax and P. ovale infections, some parasites develop into dormant liver-stage forms called hypnozoites. These parasites may remain inactive for extended periods before reactivating and initiating new blood-stage infections."
              )
            ),
            mal_detail(
              "lc-3", "Stage 3: Red blood cell infection",
              paras(
                "Once merozoites enter the bloodstream, they invade red blood cells and begin another phase of asexual reproduction. Inside red blood cells, the parasites progress through several developmental stages, including the ring-stage trophozoite, mature trophozoite, and schizont.",
                "During these stages, the parasite grows, consumes nutrients, and replicates its genetic material. The mature schizont contains multiple merozoites that are eventually released when the infected red blood cell ruptures. The newly released merozoites invade additional red blood cells, allowing the infection to continue.",
                "This repeated cycle of red blood cell invasion, parasite replication, and cell destruction is responsible for many of malaria's characteristic clinical manifestations."
              )
            ),
            mal_detail(
              "lc-4", "Stage 4: Development of gametocytes",
              paras(
                "Although most blood-stage parasites continue reproducing asexually, some differentiate into specialized sexual forms called gametocytes. Male and female gametocytes circulate in the bloodstream and can be ingested by another mosquito during a blood meal.",
                "Gametocytes are essential for transmission because they allow the parasite to continue its reproductive cycle inside the mosquito."
              )
            ),
            mal_detail(
              "lc-5", "Stage 5: Sexual reproduction inside the mosquito",
              paras(
                "When a mosquito ingests blood containing mature gametocytes, the parasites enter the mosquito's digestive tract. The gametocytes develop into male and female gametes, which fuse during fertilization to produce a zygote.",
                "The zygote develops into a motile ookinete that penetrates the mosquito's gut wall and forms an oocyst. Inside the oocyst, the parasite undergoes further development and produces numerous sporozoites. Eventually, the oocyst ruptures, releasing sporozoites that migrate to the mosquito's salivary glands.",
                "At this point, the mosquito becomes capable of transmitting malaria to another human during a subsequent blood meal."
              )
            ),
            mal_detail(
              "lc-why", "Why is the life cycle important for prevention?",
              paras(
                "The malaria parasite's life cycle provides several opportunities for prevention and treatment. Mosquito nets and insecticides reduce the likelihood of infected mosquitoes biting humans. Certain preventive medications interfere with parasite development before or during blood-stage infection. Blood-stage antimalarial medications target the parasites responsible for clinical illness, while specific medications are required to eliminate dormant liver-stage parasites in P. vivax and P. ovale infections.",
                "Vaccines may target different stages of parasite development, including the period before parasites establish infection in the liver. Because the parasite occupies different biological environments throughout its life cycle, no single intervention is sufficient to address every stage of infection and transmission."
              )
            ),
            mal_source("Source: CDC DPDx, Malaria: Life Cycle and Biology."),
            
            mal_heading(
              "Pathophysiology: How Malaria Affects the Body",
              "Interactions between parasite replication, destruction of red blood cells, inflammation, and changes in the function of small blood vessels."
            ),
            mal_cards(
              mal_card("\U0001FA78", "Red blood cell destruction",
                       "Why anemia can be severe even when few cells are infected.",
                       jump = "pa-rbc"),
              mal_card("\U0001F6E1\uFE0F", "The immune response",
                       "Essential for control, but can also cause damage.",
                       jump = "pa-immune"),
              mal_card("\U0001F9F2", "Sticky blood cells",
                       "Cytoadherence and sequestration in small vessels.",
                       jump = "pa-sequestration"),
              mal_card("\U0001F9E0", "Cerebral malaria",
                       "Among the most serious complications of P. falciparum.",
                       jump = "pa-cerebral"),
              mal_card("\U0001FAC0", "The spleen",
                       "Filters infected cells but can add to anemia.",
                       jump = "pa-spleen")
            ),
            mal_detail(
              "pa-intro", "Overview",
              paras(
                "The pathophysiology of malaria involves interactions between parasite replication, destruction of red blood cells, inflammation, and changes in the function of small blood vessels. Although different Plasmodium species produce somewhat different patterns of disease, the blood stage of infection is primarily responsible for clinical symptoms."
              )
            ),
            mal_detail(
              "pa-rbc", "Destruction of red blood cells",
              paras(
                "Malaria parasites depend on red blood cells for a major portion of their development. As parasites replicate, they alter the structure and function of infected red blood cells. Eventually, these cells rupture and release new parasites into circulation.",
                "The destruction of infected red blood cells contributes directly to anemia. However, anemia is not caused solely by the rupture of infected cells. The spleen also removes uninfected red blood cells that have become less flexible or have been altered by the inflammatory environment. At the same time, inflammation can suppress the bone marrow's ability to produce replacement red blood cells.",
                "Together, these processes can produce severe anemia even when the proportion of infected red blood cells is relatively low."
              )
            ),
            mal_detail(
              "pa-immune", "The immune response to malaria",
              paras(
                "The immune system plays an essential role in controlling malaria infection, but it can also contribute to disease severity. When parasites infect red blood cells, immune cells recognize parasite-derived molecules and initiate inflammatory responses. Macrophages, monocytes, and other immune cells release cytokines that help coordinate the body's defense against infection.",
                "These inflammatory signals contribute to fever and other systemic symptoms. Although inflammation is necessary for controlling infection, excessive or poorly regulated immune responses can contribute to tissue damage, impaired circulation, and organ dysfunction.",
                "Repeated exposure to malaria can lead to partial acquired immunity. In highly endemic regions, older children and adults may develop some protection against severe disease after repeated infections. However, this immunity is incomplete and does not necessarily prevent reinfection."
              )
            ),
            mal_detail(
              "pa-sequestration", "Cytoadherence and sequestration",
              paras(
                "One of the most important mechanisms underlying severe P. falciparum malaria is the ability of infected red blood cells to adhere to the walls of small blood vessels. The parasite produces proteins that become displayed on the surface of infected red blood cells. One particularly important family of proteins is Plasmodium falciparum erythrocyte membrane protein 1, commonly called PfEMP1.",
                "These proteins interact with receptors on endothelial cells lining blood vessels, allowing infected red blood cells to attach to vessel walls. This process, called cytoadherence, contributes to sequestration, in which infected red blood cells accumulate within small blood vessels rather than circulating freely.",
                "Sequestration can disrupt microvascular blood flow, impair oxygen delivery, activate endothelial cells, and contribute to inflammation. In severe cases, these changes may contribute to cerebral malaria, acute kidney injury, respiratory complications, and other forms of organ dysfunction."
              )
            ),
            mal_detail(
              "pa-cerebral", "Cerebral malaria",
              paras(
                "Cerebral malaria is among the most serious complications of P. falciparum infection. It is associated with the accumulation of infected red blood cells in the brain's small blood vessels, inflammatory responses, endothelial activation, and disruption of normal microvascular function. These processes may contribute to swelling, impaired brain function, seizures, and coma.",
                "Children who survive cerebral malaria may experience long-term neurological or cognitive complications, although outcomes vary considerably. Cerebral malaria demonstrates how a parasite that primarily infects red blood cells can produce severe consequences in organs throughout the body."
              )
            ),
            mal_detail(
              "pa-spleen", "The role of the spleen",
              paras(
                "The spleen plays an important role in malaria by filtering circulating blood and removing abnormal or infected red blood cells. During infection, immune activity within the spleen increases, and the organ may become enlarged.",
                "Although this filtering process helps control parasite levels, it can also contribute to anemia by removing red blood cells from circulation. In some cases, repeated or chronic malaria infections may produce substantial enlargement of the spleen."
              )
            ),
            mal_source("Sources: CDC, Clinical Features of Malaria; WHO, Guidelines for Malaria; reviews of malaria pathogenesis and severe falciparum malaria."),
            
            mal_next(
              mal_card("\U0001F4DC", "History of malaria",
                       "How the parasite and the mosquito were discovered.",
                       tab = "History of Malaria", hint = "Open \u2192"),
              mal_card("\U0001F30D", "Where malaria occurs",
                       "Geography, burden, and the countries hit hardest.",
                       tab = "Geographic Distribution", hint = "Open \u2192"),
              mal_card("\U0001F6E1\uFE0F", "Preventing and treating it",
                       "Every stage of the life cycle is a chance to intervene.",
                       nav = "Treatment", tab = "Treatment Options", hint = "Open \u2192")
            )
          )
        ),
        
        # ======================================================
        # HISTORY (timeline + yearly trend)
        # ======================================================
        tabPanel(
          "History of Malaria",
          br(),
          mal_page(
            
            mal_hero(
              "Thousands of years of malaria",
              "History of Malaria",
              "Malaria has shaped human populations, medicine, and public health for thousands of years."
            ),
            
            mal_heading("Timeline", "Click an event to read the story behind it."),
            mal_timeline(
              mal_event("Ancient", "Early descriptions",
                        "Illnesses resembling malaria appear in ancient medical writings from China, Greece, and other civilizations.",
                        jump = "hi-ancient"),
              mal_event("Bad air", "Where the name comes from",
                        "Malaria comes from the Italian mal aria, meaning bad air, reflecting an old belief about marshes.",
                        jump = "hi-name"),
              mal_event("Before 1880", "Quinine",
                        "Cinchona bark preparations treated malaria long before the parasite was discovered.",
                        jump = "hi-quinine"),
              mal_event("1880", "The parasite is found",
                        "Alphonse Laveran identifies parasites in the blood of malaria patients.",
                        jump = "hi-laveran"),
              mal_event("1897", "Mosquitoes spread it",
                        "Ronald Ross shows parasite development in mosquitoes, and Italian scientists confirm the role of Anopheles.",
                        jump = "hi-ross"),
              mal_event("1927", "Malaria as a treatment",
                        "Julius Wagner-Jauregg wins a Nobel Prize for using malaria fever to treat neurosyphilis.",
                        jump = "hi-therapy"),
              mal_event("1955", "Global eradication campaign",
                        "WHO launches the Global Malaria Eradication Programme, using tools such as DDT.",
                        jump = "hi-twentieth"),
              mal_event("1970s", "Artemisinin",
                        "Tu Youyou and colleagues help develop artemisinin, which earned a Nobel Prize in 2015.",
                        jump = "hi-artemisinin"),
              mal_event("2021\u20132023", "Malaria vaccines",
                        "WHO recommends RTS,S/AS01 in 2021 and R21/Matrix-M in 2023.",
                        jump = "hi-modern")
            ),
            
            mal_heading("The Full Story"),
            mal_detail(
              "hi-ancient", "Ancient descriptions", open = TRUE,
              paras(
                "Malaria has influenced human populations for thousands of years and has played an important role in the development of medicine, public health, and infectious disease research.",
                "Descriptions of illnesses resembling malaria appear in ancient medical writings from China, Greece, and other civilizations. Although these early accounts cannot always be definitively linked to malaria, they describe recurring fevers and other symptoms consistent with the disease. Evidence of malaria infection has also been identified in ancient human remains through molecular and other scientific analyses."
              )
            ),
            mal_detail(
              "hi-name", "Origins of the name",
              paras(
                "The word malaria comes from the Italian words mal aria, meaning \u201cbad air.\u201d For centuries, people believed that malaria was caused by unhealthy air associated with marshes and wetlands.",
                "This explanation reflected the observation that malaria frequently occurred near stagnant water, although the underlying relationship between mosquito breeding habitats and disease transmission was not yet understood. The development of germ theory and parasitology eventually replaced the idea that malaria was caused directly by contaminated air."
              )
            ),
            mal_detail(
              "hi-quinine", "Quinine and early antimalarial treatments",
              paras(
                "Long before the malaria parasite was discovered, quinine-containing preparations derived from cinchona bark were used to treat malaria. Quinine became one of the most important antimalarial medications in history and was widely used before the development of newer drugs.",
                "Its use illustrates how plant-derived compounds contributed to the development of modern infectious disease treatments."
              )
            ),
            mal_detail(
              "hi-laveran", "Discovery of the malaria parasite (1880)",
              paras(
                "In 1880, French military physician Alphonse Laveran identified parasites in the blood of patients suffering from malaria. His discovery established that malaria was caused by a living microorganism rather than environmental air alone.",
                "Laveran's research helped establish the scientific foundation for understanding malaria as a parasitic disease. He later received the Nobel Prize in Physiology or Medicine in 1907 for his work on protozoan parasites."
              )
            ),
            mal_detail(
              "hi-ross", "Discovery of mosquito transmission (1897)",
              paras(
                "In 1897, British physician Ronald Ross demonstrated important stages of malaria parasite development in mosquitoes. His research provided crucial evidence linking mosquitoes to malaria transmission.",
                "Subsequent work by Italian scientists, including Giovanni Battista Grassi and colleagues, helped establish the role of Anopheles mosquitoes in transmitting human malaria. These discoveries transformed malaria prevention by demonstrating that controlling mosquito populations could reduce transmission. Ross received the Nobel Prize in Physiology or Medicine in 1902."
              )
            ),
            mal_detail(
              "hi-therapy", "Malaria therapy for neurosyphilis",
              paras(
                "During the early twentieth century, malaria was deliberately used as a treatment for neurosyphilis, a serious complication of untreated syphilis. Austrian physician Julius Wagner-Jauregg developed malaria inoculation therapy based on the observation that high fevers could sometimes improve symptoms in patients with certain neurological conditions.",
                "Patients were intentionally infected with malaria to induce fever and were subsequently treated for the malaria infection. Although this treatment was dangerous and is no longer used, it represented an important historical development in medicine. Wagner-Jauregg received the Nobel Prize in Physiology or Medicine in 1927 for his work.",
                "The approach was eventually replaced by safer and more effective treatments, particularly following the introduction of penicillin."
              )
            ),
            mal_detail(
              "hi-twentieth", "Twentieth-century malaria control",
              paras(
                "During the twentieth century, malaria control efforts increasingly emphasized mosquito control, improved sanitation, environmental management, and antimalarial medications. The introduction of synthetic insecticides such as DDT contributed to major reductions in mosquito populations in some regions.",
                "In 1955, the World Health Organization launched the Global Malaria Eradication Programme. The program achieved important successes in several regions but ultimately failed to eliminate malaria worldwide. Challenges included insecticide resistance, drug resistance, inadequate funding, operational difficulties, and the complexity of transmission in highly endemic regions."
              )
            ),
            mal_detail(
              "hi-artemisinin", "Discovery of artemisinin",
              paras(
                "During the 1970s, Chinese scientist Tu Youyou and her colleagues helped develop artemisinin from the plant Artemisia annua. Artemisinin and its derivatives became central components of modern antimalarial treatment because of their ability to rapidly reduce parasite concentrations.",
                "Tu Youyou received the 2015 Nobel Prize in Physiology or Medicine for her discoveries concerning a novel therapy against malaria."
              )
            ),
            mal_detail(
              "hi-modern", "Twenty-first-century advances",
              paras(
                "Modern malaria research has focused on developing more effective medications, improving diagnostics, expanding mosquito-control strategies, and producing vaccines. In 2021, WHO recommended the RTS,S/AS01 malaria vaccine for children in eligible malaria-endemic settings. In 2023, WHO recommended a second malaria vaccine, R21/Matrix-M.",
                "These developments represent important milestones in efforts to reduce malaria illness and mortality. Despite scientific progress, malaria remains a major public health challenge, demonstrating that disease control depends on both biomedical innovations and effective healthcare systems."
              )
            ),
            mal_source("Sources: Nobel Prize historical archives; CDC, History of Malaria; WHO, World Malaria Report."),
            
            # Yearly trend chart
            jump_section(
              "hi-trend",
              mal_heading(
                "Malaria Since 2000",
                "The line shows the median country's incidence each year. The shaded band covers the middle half of countries (25th to 75th percentile). Pick a country to compare it with the global picture."
              ),
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
            ),
            
            mal_next(
              mal_card("\U0001F30D", "Where malaria occurs",
                       "Geography, burden, and the countries hit hardest.",
                       tab = "Geographic Distribution", hint = "Open \u2192"),
              mal_card("\U0001F465", "Who is most affected",
                       "Age, pregnancy, poverty, and access to care.",
                       tab = "Who Is Most Affected", hint = "Open \u2192"),
              mal_card("\U0001F33F", "Artemisinin today",
                       "How the 1970s discovery shapes modern treatment.",
                       nav = "Treatment", tab = "Treatment Options", hint = "Open \u2192")
            )
          )
        ),
        
        # ======================================================
        # GEOGRAPHIC DISTRIBUTION (+ burden + charts)
        # ======================================================
        tabPanel(
          "Geographic Distribution",
          br(),
          mal_page(
            
            mal_hero(
              "Where malaria occurs",
              "Geographic Distribution & Global Burden",
              "Malaria is concentrated in tropical and subtropical regions, and the burden is far from evenly shared."
            ),
            
            mal_heading("Key Points", "Click a card to read about each region."),
            mal_cards(
              mal_card("\U0001F30D", "Sub-Saharan Africa",
                       "About 94% of cases and 95% of deaths in 2024.",
                       jump = "ge-africa"),
              mal_card("\U0001F30F", "Asia",
                       "Both P. falciparum and P. vivax, plus zoonotic P. knowlesi.",
                       jump = "ge-asia"),
              mal_card("\U0001F30E", "Central & South America",
                       "Forest regions, with the Amazon Basin a key area.",
                       jump = "ge-americas"),
              mal_card("\U0001F5FA\uFE0F", "Europe & North America",
                       "Historically widespread, now eliminated from sustained transmission.",
                       jump = "ge-europe"),
              mal_card("\U0001F321\uFE0F", "Environmental factors",
                       "Temperature, rainfall, humidity, altitude, and human activity.",
                       jump = "ge-environment"),
              mal_card("\U0001F4CD", "See it on the map",
                       "Explore incidence for every country, year by year.",
                       nav = "Home Page", hint = "Open the map \u2192"),
              mal_card("\U0001F4CA", "Most common countries chart",
                       "Rank the countries with the highest incidence in any year.",
                       jump = "ge-top"),
              mal_card("\U0001F19A", "Compare countries",
                       "Plot incidence over time for any countries you choose.",
                       jump = "ge-compare")
            ),
            
            mal_heading("Geographic Distribution"),
            mal_detail(
              "ge-intro", "Overview", open = TRUE,
              paras(
                "Malaria is primarily found in tropical and subtropical regions where environmental conditions support the survival of Anopheles mosquitoes and the development of Plasmodium parasites. The geographic distribution of malaria is influenced by temperature, rainfall, humidity, altitude, land use, mosquito ecology, and human activity.",
                "Although malaria occurs across several continents, the disease burden is not distributed equally."
              )
            ),
            mal_detail(
              "ge-africa", "Malaria in sub-Saharan Africa",
              paras(
                "Sub-Saharan Africa experiences the greatest burden of malaria worldwide. According to the World Health Organization's World Malaria Report 2025, the WHO African Region accounted for approximately 94% of global malaria cases and 95% of malaria deaths in 2024. Several factors contribute to this disproportionate burden.",
                "First, environmental conditions in many regions support the survival and reproduction of highly efficient malaria vectors, including mosquitoes belonging to the Anopheles gambiae species complex. Second, P. falciparum, the parasite responsible for most malaria deaths, is especially prevalent in this region. Third, socioeconomic conditions and healthcare disparities can limit access to mosquito nets, diagnostic testing, preventive medications, and effective treatment.",
                "Young children are particularly vulnerable because they have not yet developed the partial immunity that may arise from repeated malaria exposure."
              )
            ),
            mal_detail(
              "ge-asia", "Malaria in Asia",
              paras(
                "Malaria remains endemic in parts of South Asia, Southeast Asia, and the Western Pacific. Both P. falciparum and P. vivax contribute to malaria transmission in these regions. P. vivax is particularly important because its ability to form dormant liver-stage parasites complicates elimination efforts.",
                "In Southeast Asia, zoonotic malaria caused by P. knowlesi also presents a public health concern, especially in areas where human activities overlap with the habitats of macaque monkeys and mosquito vectors. Changes in land use, including deforestation and agricultural development, can alter interactions among humans, mosquitoes, and animal hosts."
              )
            ),
            mal_detail(
              "ge-americas", "Malaria in Central and South America",
              paras(
                "Malaria occurs in parts of Central and South America, particularly in regions associated with tropical forests and remote communities. The Amazon Basin represents an important area of malaria transmission. In many parts of the Americas, P. vivax accounts for a substantial proportion of infections, although P. falciparum also occurs.",
                "Malaria control in these regions can be complicated by geographic isolation, limited transportation infrastructure, population movement, and occupational exposure associated with mining, agriculture, and forest-related activities."
              )
            ),
            mal_detail(
              "ge-europe", "Malaria in Europe and North America",
              paras(
                "Malaria was historically present in parts of Europe and North America, including the United States. During the nineteenth and early twentieth centuries, malaria transmission occurred in several areas of the United States, particularly in the southeastern states. Improvements in mosquito control, housing, drainage, healthcare, and public health infrastructure contributed to the eventual elimination of sustained malaria transmission. The United States was declared malaria-free in 1970.",
                "Today, most malaria cases diagnosed in the United States are associated with international travel, although occasional locally acquired mosquito-borne cases can occur. The continued presence of some Anopheles mosquito species means that limited local transmission remains biologically possible when infected individuals and suitable mosquito vectors are present."
              )
            ),
            mal_detail(
              "ge-environment", "Environmental factors influencing malaria distribution",
              paras(
                "Temperature affects both mosquito development and parasite reproduction. Warmer conditions can accelerate certain stages of mosquito and parasite development, although extremely high temperatures may reduce mosquito survival. Rainfall can create breeding habitats, but the relationship between rainfall and malaria transmission depends on local conditions.",
                "Humidity influences mosquito survival because mosquitoes are susceptible to dehydration. Altitude can also affect malaria distribution because cooler temperatures at higher elevations may limit mosquito activity and parasite development.",
                "Human activities, including irrigation, dam construction, agricultural expansion, deforestation, and urban development, can create or modify mosquito breeding environments. As a result, malaria transmission is shaped by interactions between ecological conditions and human social systems."
              )
            ),
            mal_source("Sources: WHO, Global Malaria Observatory; WHO, World Malaria Report 2025."),
            
            mal_heading("Global Burden and Epidemiology"),
            mal_stats(
              mal_stat("282 million", "estimated cases in 2024 (about 273 million in 2023)"),
              mal_stat("610,000", "estimated deaths in 2024"),
              mal_stat("59 \u2192 64", "cases per 1,000 at risk, 2015 to 2024"),
              mal_stat("14.9 \u2192 13.8", "deaths per 100,000 at risk, 2015 to 2024")
            ),
            br(),
            mal_detail(
              "ge-epi", "What epidemiology tells us",
              paras(
                "Malaria remains one of the world's most significant infectious disease challenges, affecting millions of people and placing substantial pressure on healthcare systems. Epidemiology examines the distribution, frequency, and determinants of disease within populations. Understanding malaria epidemiology helps researchers and public health organizations identify vulnerable populations, monitor changes in transmission, and evaluate prevention programs.",
                "According to the World Health Organization's World Malaria Report 2025, there were an estimated 282 million malaria cases worldwide in 2024, compared with approximately 273 million in 2023. An estimated 610,000 people died from malaria in 2024. These estimates demonstrate that malaria continues to cause substantial illness and mortality despite the availability of effective prevention and treatment strategies.",
                "The WHO African Region accounted for approximately 94% of malaria cases and 95% of malaria deaths worldwide in 2024. Children under five accounted for approximately 75% of malaria deaths in the WHO African Region. The concentration of malaria deaths among young children reflects the importance of age, acquired immunity, exposure, and access to preventive healthcare."
              )
            ),
            mal_detail(
              "ge-incidence", "Malaria incidence and mortality",
              paras(
                "Malaria incidence refers to the number of new malaria cases occurring within a population during a specified period. Malaria mortality refers to deaths attributed to malaria. These measures are important because the total number of cases or deaths can change as populations grow, even when the risk experienced by an individual changes differently.",
                "According to WHO, global malaria incidence increased from approximately 59 cases per 1,000 population at risk in 2015 to 64 cases per 1,000 in 2024. Over the same period, the global malaria mortality rate declined from approximately 14.9 to 13.8 deaths per 100,000 population at risk.",
                "These trends illustrate the complexity of malaria control: progress in reducing mortality rates can occur even as the total number of cases and deaths remains high or increases."
              )
            ),
            mal_detail(
              "ge-progress", "Progress in malaria control",
              paras(
                "Since 2000, global malaria prevention and treatment efforts have prevented substantial numbers of infections and deaths. According to WHO, approximately 2.3 billion malaria cases and 14 million deaths were averted worldwide between 2000 and 2024.",
                "These achievements reflect the expansion of insecticide-treated mosquito nets, improved diagnostic testing, effective antimalarial medications, and strengthened public health programs. However, progress has been uneven, and some regions have experienced stagnation or increases in malaria transmission. Challenges include drug resistance, insecticide resistance, population displacement, conflict, climate-related disruptions, and insufficient funding."
              )
            ),
            mal_source("Source: WHO, World Malaria Report 2025."),
            
            # Highest-burden chart
            jump_section(
              "ge-top",
              mal_heading(
                "Where Is Malaria Most Common?",
                "The countries with the highest incidence per 1,000 people at risk in the year you choose."
              ),
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
              back_link("ge-intro", "\u2191 Back to the regions")
            ),
            
            # Country comparison chart
            jump_section(
              "ge-compare",
              mal_heading(
                "Compare Countries",
                "Select two or more countries to compare incidence over time."
              ),
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
                  div(class = "ds-panel", tableOutput("compare_table"))
                )
              )
            ),
            
            mal_next(
              mal_card("\U0001F465", "Who is most affected",
                       "Why the burden falls on some people more than others.",
                       tab = "Who Is Most Affected", hint = "Open \u2192"),
              mal_card("\U0001F1EA\U0001F1FA", "Eliminating malaria in Europe",
                       "A case study of a region that reached zero indigenous cases.",
                       nav = "Comparative Case Study (2000-2015)", hint = "Open \u2192"),
              mal_card("\U0001F326\uFE0F", "Climate & resistance",
                       "How a changing climate may move the map.",
                       tab = "Climate & Resistance", hint = "Open \u2192")
            )
          )
        ),
        
        # ======================================================
        # WHO IS MOST AFFECTED (vulnerable populations + poverty)
        # ======================================================
        tabPanel(
          "Who Is Most Affected",
          br(),
          mal_page(
            
            mal_hero(
              "People and inequality",
              "Who Is Most Affected",
              "Malaria is not only a biological problem. Poverty, age, pregnancy, and access to healthcare decide who gets sick and who survives."
            ),
            
            mal_heading("Populations at Greatest Risk", "Click a card to learn why."),
            mal_cards(
              mal_card("\U0001F476", "Children under five",
                       "Without acquired immunity, infections can progress rapidly.",
                       jump = "wh-children"),
              mal_card("\U0001F930", "Pregnant women",
                       "Parasites can accumulate in the placenta.",
                       jump = "wh-pregnant"),
              mal_card("\U0001F9F3", "Travelers",
                       "No partial immunity from repeated exposure.",
                       jump = "wh-travelers"),
              mal_card("\U0001F3D5\uFE0F", "Displaced populations",
                       "Crowding, interrupted care, and limited nets or medicines.",
                       jump = "wh-displaced"),
              mal_card("\U0001F4B8", "People living in poverty",
                       "More exposure, less access to care.",
                       jump = "wh-poverty"),
              mal_card("\U0001F501", "The cycle of poverty",
                       "How malaria and poverty reinforce each other.",
                       jump = "po-cycle-steps")
            ),
            
            mal_detail(
              "wh-intro", "Why vulnerability differs", open = TRUE,
              paras(
                "Although malaria can affect individuals of all ages, some populations face a greater risk of infection, severe complications, or death. Differences in vulnerability are influenced by immune status, pregnancy, age, healthcare access, and environmental exposure."
              )
            ),
            mal_detail(
              "wh-children", "Children under five",
              paras(
                "Children under five are among the populations most vulnerable to severe malaria. In areas with frequent malaria transmission, older children and adults may develop partial immunity through repeated exposure to the parasite. Young children generally have not experienced enough exposure to develop comparable immune protection.",
                "Consequently, malaria infections in young children may progress rapidly and produce severe anemia, respiratory distress, cerebral malaria, or death. Malaria can also interfere with children's education and development through repeated illness, missed school days, and potential long-term neurological consequences following severe infection."
              )
            ),
            mal_detail(
              "wh-pregnant", "Pregnant women",
              paras(
                "Pregnancy increases vulnerability to malaria and its complications. During pregnancy, malaria parasites, particularly P. falciparum, can accumulate in the placenta. Placental malaria can interfere with normal placental function and contribute to maternal anemia, fetal growth restriction, low birth weight, premature delivery, and other adverse pregnancy outcomes.",
                "Malaria prevention during pregnancy is therefore an important component of maternal and child healthcare in endemic regions."
              )
            ),
            mal_detail(
              "wh-travelers", "Travelers and individuals without acquired immunity",
              paras(
                "Individuals traveling from regions without malaria transmission to endemic areas may be especially vulnerable because they have not developed partial immunity through repeated exposure. Travelers may also be unfamiliar with malaria symptoms or preventive strategies.",
                "Preventive medications, mosquito bite avoidance, and prompt medical evaluation for fever after travel are important measures for reducing risk."
              )
            ),
            mal_detail(
              "wh-displaced", "Displaced populations and refugees",
              paras(
                "Refugees and displaced populations may face elevated malaria risk because of overcrowded living conditions, inadequate shelter, interrupted healthcare services, and limited access to mosquito nets or medications.",
                "Conflict and humanitarian emergencies can disrupt malaria surveillance and prevention programs, allowing transmission to increase. Population movement can also introduce malaria parasites into areas where suitable mosquito vectors are present."
              )
            ),
            mal_detail(
              "wh-poverty", "People living in poverty",
              paras(
                "Individuals living in poverty may experience increased exposure to mosquitoes and greater difficulty accessing preventive healthcare. Housing conditions, limited financial resources, transportation barriers, and healthcare costs can all influence malaria outcomes.",
                "Poverty can also increase the consequences of infection because illness may prevent individuals from working or attending school."
              )
            ),
            mal_source("Source: WHO, Malaria Questions and Answers."),
            
            jump_section(
              "po-cycle-steps",
              mal_heading(
                "Malaria, Poverty, and Global Health Inequality",
                "The relationship often runs in both directions. Click a step of the cycle to read more."
              ),
              mal_steps(
                mal_step("1", "Limited resources", "Less access to nets, medicines, and care.", jump = "po-risk"),
                mal_step("2", "Malaria infection", "Delayed diagnosis makes severe disease more likely.", jump = "po-risk"),
                mal_step("3", "Costs and lost income", "Medical bills, missed work, missed school.", jump = "po-economic"),
                mal_step("4", "Deeper hardship", "Fewer resources for the next prevention or treatment.", jump = "po-cycle")
              )
            ),
            mal_detail(
              "po-risk", "How poverty increases malaria risk",
              paras(
                "Individuals living in poverty may have limited access to protective housing, insecticide-treated mosquito nets, preventive medications, and healthcare services. In some communities, housing structures contain openings that allow mosquitoes to enter easily.",
                "Families may also lack the financial resources needed to travel to healthcare facilities or obtain timely medical evaluation. Geographic isolation can create additional barriers, particularly in rural communities where healthcare facilities are far from residential areas. When diagnosis and treatment are delayed, malaria infections are more likely to progress to severe disease."
              )
            ),
            mal_detail(
              "po-economic", "The economic burden on households",
              paras(
                "Malaria can impose direct and indirect financial costs on affected households. Direct costs include transportation to healthcare facilities, medications, diagnostic testing, hospitalization, and other medical expenses. Indirect costs arise when individuals miss work, caregivers must remain home with sick children, or household members lose income while recovering from illness.",
                "For families with limited financial resources, even relatively small medical expenses can create substantial economic strain. Repeated malaria infections may therefore contribute to cycles of illness and financial insecurity."
              )
            ),
            mal_detail(
              "po-education", "Malaria and education",
              paras(
                "Malaria can negatively affect educational participation and achievement. Children experiencing malaria may miss school because of fever, fatigue, or hospitalization. Repeated infections can interfere with attendance and learning. In severe cases, neurological complications may produce lasting cognitive or developmental difficulties.",
                "Malaria can also affect teachers and caregivers, disrupting educational systems and household support. Reducing malaria transmission may therefore provide benefits that extend beyond improved physical health."
              )
            ),
            mal_detail(
              "po-work", "Malaria and workforce productivity",
              paras(
                "Malaria can reduce economic productivity when infected individuals are unable to work or must care for sick family members. In agricultural communities, illness during planting or harvesting periods can interfere with food production and household income.",
                "Businesses may also experience reduced productivity because of employee illness and absenteeism. At a broader level, persistent malaria transmission can place pressure on healthcare systems and limit resources available for other public health priorities."
              )
            ),
            mal_detail(
              "po-inequality", "Healthcare inequality",
              paras(
                "Access to malaria prevention and treatment differs substantially between and within countries. Some communities have reliable access to diagnostic testing, medications, vaccination programs, and trained healthcare personnel. Others face shortages of medical supplies, limited laboratory capacity, and inadequate transportation infrastructure.",
                "These disparities can produce major differences in malaria outcomes even when communities experience similar environmental exposure. Addressing malaria therefore requires investments in healthcare accessibility, infrastructure, education, and social conditions alongside biomedical interventions."
              )
            ),
            mal_detail(
              "po-cycle", "The cycle of malaria and poverty",
              paras(
                "Malaria and poverty can reinforce one another through a recurring cycle. Limited financial resources can reduce access to prevention and healthcare, increasing vulnerability to malaria. Infection can then produce medical expenses, lost wages, missed school days, and reduced productivity.",
                "These consequences may further limit household resources, making future prevention and treatment more difficult. Breaking this cycle requires coordinated approaches that address both disease transmission and the social conditions that influence vulnerability."
              )
            ),
            
            mal_next(
              mal_card("\U0001F326\uFE0F", "Climate & resistance",
                       "Moving targets that make malaria harder to control.",
                       tab = "Climate & Resistance", hint = "Open \u2192"),
              mal_card("\U0001F1EA\U0001F1FA", "A success story",
                       "How the WHO European Region eliminated indigenous malaria.",
                       nav = "Comparative Case Study (2000-2015)", hint = "Open \u2192"),
              mal_card("\U0001F48A", "Treatment options",
                       "The drugs and guidelines behind modern care.",
                       nav = "Treatment", tab = "Treatment Options", hint = "Open \u2192")
            )
          )
        ),
        
        # ======================================================
        # CLIMATE & RESISTANCE
        # ======================================================
        tabPanel(
          "Climate & Resistance",
          br(),
          mal_page(
            
            mal_hero(
              "Moving targets",
              "Climate & Resistance",
              "Changing weather patterns and evolving resistance to drugs and insecticides make malaria harder to control."
            ),
            
            mal_heading("Malaria and Climate Change", "Click a card to read more."),
            mal_cards(
              mal_card("\U0001F321\uFE0F", "Temperature",
                       "Warmer is not always worse. Extreme heat can reduce mosquito survival.",
                       jump = "cl-temp"),
              mal_card("\U0001F327\uFE0F", "Rainfall",
                       "Can create or wash away breeding habitats.",
                       jump = "cl-rain"),
              mal_card("\U0001F5FA\uFE0F", "Shifting range",
                       "Some areas become more suitable, others less.",
                       jump = "cl-range"),
              mal_card("\U0001F32A\uFE0F", "Extreme weather",
                       "Floods and storms can disrupt healthcare and displace people.",
                       jump = "cl-extreme"),
              mal_card("\u2696\uFE0F", "Not the only factor",
                       "Economics, housing, and public health investment matter too.",
                       jump = "cl-other")
            ),
            mal_detail(
              "cl-intro", "Overview", open = TRUE,
              paras(
                "Climate change has the potential to influence malaria transmission by altering environmental conditions that affect mosquitoes, parasites, and human populations. However, the relationship between climate change and malaria is complex and varies across geographic regions."
              )
            ),
            mal_detail(
              "cl-temp", "Temperature and mosquito development",
              paras(
                "Temperature influences mosquito survival, reproduction, and development. It also affects how quickly malaria parasites develop inside mosquitoes. Within suitable temperature ranges, warmer conditions may accelerate parasite development and potentially increase transmission.",
                "However, extremely high temperatures can reduce mosquito survival and may decrease transmission under certain circumstances. Therefore, rising temperatures do not necessarily increase malaria risk in every location."
              )
            ),
            mal_detail(
              "cl-rain", "Rainfall and mosquito breeding habitats",
              paras(
                "Rainfall can influence the availability of standing water where mosquitoes lay their eggs. In some environments, increased rainfall creates additional breeding habitats. In other settings, heavy rainfall may wash away larvae or disrupt existing breeding sites.",
                "Drought can also alter mosquito habitats by changing water storage practices and concentrating water in particular locations. The effects of precipitation on malaria therefore depend on local ecology and human behavior."
              )
            ),
            mal_detail(
              "cl-range", "Changes in geographic distribution",
              paras(
                "Changing temperatures and rainfall patterns may alter the geographic range or seasonal activity of malaria-transmitting mosquitoes. Some regions that were previously too cool for sustained transmission may become more environmentally suitable. Conversely, other areas may become less suitable because of extreme heat or changing rainfall patterns.",
                "Changes in transmission also depend on healthcare infrastructure, land use, mosquito control, population movement, and existing malaria prevention programs."
              )
            ),
            mal_detail(
              "cl-extreme", "Extreme weather and healthcare disruptions",
              paras(
                "Flooding, storms, droughts, and other environmental disruptions can affect malaria risk indirectly. Extreme weather events may displace populations, damage healthcare facilities, interrupt medication supplies, and reduce access to preventive services.",
                "These disruptions can make malaria prevention and treatment more difficult, even when environmental conditions do not directly increase mosquito populations."
              )
            ),
            mal_detail(
              "cl-other", "Why climate change is not the only factor",
              paras(
                "Climate change is one influence among many that determine malaria transmission. Economic development, housing quality, public health investment, mosquito-control programs, healthcare accessibility, and human movement also play major roles.",
                "Understanding future malaria patterns therefore requires examining both environmental changes and the social systems that shape human exposure and vulnerability."
              )
            ),
            
            mal_heading(
              "Drug Resistance and Insecticide Resistance",
              "Resistance arises through evolution: organisms with traits that improve survival become more common over time."
            ),
            mal_cards(
              mal_card("\U0001F9EA", "Chloroquine resistance",
                       "Once a mainstay drug, now ineffective against P. falciparum in many places.",
                       jump = "re-chloroquine"),
              mal_card("\U0001F33F", "Artemisinin partial resistance",
                       "Confirmed or suspected in at least eight African countries.",
                       jump = "re-artemisinin"),
              mal_card("\U0001F99F", "Insecticide resistance",
                       "Mosquitoes that survive nets and spraying.",
                       jump = "re-insecticide"),
              mal_card("\U0001F6E0\uFE0F", "Managing resistance",
                       "Surveillance, combination therapy, and newer nets.",
                       jump = "re-manage"),
              mal_card("\U0001F4CA", "See resistance in the data",
                       "Explore WHO therapeutic efficacy studies by drug and country.",
                       nav = "Treatment", tab = "Resistance and Mutations", hint = "Open the dashboard \u2192")
            ),
            mal_detail(
              "re-intro", "Overview", open = TRUE,
              paras(
                "Drug resistance and insecticide resistance represent major challenges to global malaria control and elimination efforts. Both forms of resistance arise through evolutionary processes in which organisms possessing traits that improve survival become more common over time."
              )
            ),
            mal_detail(
              "re-drug", "Antimalarial drug resistance",
              paras(
                "Antimalarial drug resistance occurs when malaria parasites develop the ability to survive or reproduce despite exposure to medications that would ordinarily eliminate them. Resistance can emerge through genetic mutations that alter drug targets, affect drug transport, or change parasite responses to treatment.",
                "When resistant parasites survive treatment, they may continue reproducing and potentially spread through mosquito transmission."
              )
            ),
            mal_detail(
              "re-chloroquine", "Chloroquine resistance",
              paras(
                "Chloroquine was historically one of the most widely used antimalarial medications. However, resistance developed in P. falciparum populations and spread across many malaria-endemic regions. As resistance increased, chloroquine became ineffective against P. falciparum in many locations.",
                "This development contributed to the adoption of alternative medications and eventually the widespread use of artemisinin-based combination therapies."
              )
            ),
            mal_detail(
              "re-artemisinin", "Artemisinin partial resistance",
              paras(
                "Artemisinin derivatives are central components of modern malaria treatment. However, parasites with reduced susceptibility to artemisinin have emerged in several regions. Artemisinin partial resistance is generally associated with delayed clearance of parasites from the bloodstream following treatment.",
                "It does not necessarily mean that combination therapy will fail, because the partner medication may still eliminate remaining parasites. Nevertheless, the spread of resistance threatens treatment effectiveness, particularly when resistance to partner medications also develops. WHO's World Malaria Report 2025 identifies artemisinin partial resistance as confirmed or suspected in at least eight African countries."
              )
            ),
            mal_detail(
              "re-insecticide", "Insecticide resistance",
              paras(
                "Insecticide resistance occurs when mosquito populations develop the ability to survive exposure to insecticides. Repeated insecticide use can create selection pressure favoring mosquitoes with resistance-associated genetic or physiological traits.",
                "Resistance mechanisms may include changes in insecticide target sites, increased metabolic breakdown of insecticides, and behavioral adaptations. Resistance can reduce the effectiveness of mosquito-control interventions, including some insecticide-treated nets and indoor residual spraying programs."
              )
            ),
            mal_detail(
              "re-manage", "Strategies for managing resistance",
              paras(
                "Managing resistance requires continued surveillance, appropriate medication use, and adaptation of prevention strategies. Public health programs monitor antimalarial treatment effectiveness and mosquito susceptibility to insecticides.",
                "Combination therapies help reduce the likelihood that parasites resistant to one medication will survive treatment. Newer mosquito nets incorporating multiple active ingredients or insecticide combinations may help address certain forms of insecticide resistance. Continued development of new medications, insecticides, and prevention technologies is essential for sustaining malaria control."
              )
            ),
            mal_source("Source: WHO, World Malaria Report 2025."),
            
            mal_next(
              mal_card("\U0001F48A", "Treatment options",
                       "Which drugs are used and how guidelines are chosen.",
                       nav = "Treatment", tab = "Treatment Options", hint = "Open \u2192"),
              mal_card("\u26A0\uFE0F", "Threats to control",
                       "Invasive mosquitoes, resistance, and gene deletions.",
                       nav = "Treatment", tab = "Threats to Controlling Malaria", hint = "Open \u2192"),
              mal_card("\U0001F5FA\uFE0F", "Resistance map",
                       "Explore WHO efficacy studies by drug and country.",
                       nav = "Treatment", tab = "Resistance and Mutations", hint = "Open \u2192")
            )
          )
        )
        
      )
    )
  ),
  
  # ---------------- COMPARATIVE CASE STUDY (one page) ----------------
  
  tabPanel(
    "Comparative Case Study (2000-2015)",
    fluidPage(
      
      # Styles for this page only (all classes start with "cs-")
      tags$style(HTML("
        .cs-year-bar {
          position: sticky;
          top: 0;
          z-index: 1000;
          max-width: 1800px;
          margin: 0 auto 28px auto;
          padding: 14px 24px 2px 24px;
          background: #312c54;
          border: 1px solid rgba(255, 173, 229, 0.35);
          border-radius: 14px;
          box-shadow: 0 6px 18px rgba(0, 0, 0, 0.35);
        }
        .cs-year-bar .form-group {
          margin-bottom: 6px;
        }
        .cs-two-col {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(380px, 1fr));
          gap: 20px;
          margin-top: 10px;
        }
        .cs-panel {
          background: rgba(255, 173, 229, 0.05);
          border: 1px solid rgba(255, 173, 229, 0.25);
          border-radius: 16px;
          padding: 20px;
        }
        .cs-panel h4 {
          color: #FFADE5;
          margin-top: 0;
        }
        .cs-panel p, .cs-panel .help-block {
          color: #e6c3dc;
        }
        .cs-text {
          color: #e6c3dc;
          line-height: 1.7;
          max-width: 1000px;
        }
      ")),
      
      # ---- Hero ----
      jump_section(
        "cs-top",
        div(
          class = "home-hero",
          div(class = "home-kicker", "Comparative case study"),
          h1("Malaria Elimination in Europe"),
          p(
            "WHO European Region, 2000\u20132015. Explore how estimated malaria ",
            "cases fell across the region, with a closer look at Azerbaijan and ",
            "at healthcare resources."
          )
        )
      ),
      
      # ---- Jump cards ----
      div(
        class = "home-section",
        h3("In This Case Study"),
        p(class = "home-lead", "Click a card to jump to a section. The year slider below stays with you as you scroll."),
        mal_cards(
          mal_card("\U0001F5FA\uFE0F", "Map of Europe",
                   "Estimated cases by country, year by year.",
                   jump = "cs-map"),
          mal_card("\U0001F1E6\U0001F1FF", "Focus: Azerbaijan",
                   "Milestones on the road to a malaria-free certification.",
                   jump = "cs-az"),
          mal_card("\U0001F4C9", "Cases over time",
                   "Available global estimates compared with Europe.",
                   jump = "cs-cases"),
          mal_card("\U0001FA7A", "Healthcare resources",
                   "Doctors per 10,000 people against malaria cases.",
                   jump = "cs-resources"),
          mal_card("\U0001F30D", "Malaria worldwide",
                   "See how the rest of the world compares.",
                   nav = "What is Malaria?", tab = "Geographic Distribution",
                   hint = "Open \u2192")
        )
      ),
      
      # ---- Shared year slider (stays at the top while scrolling) ----
      div(
        class = "cs-year-bar",
        sliderInput(
          "eu_year", "Explore the year:",
          min = 2000, max = 2015, value = 2000,
          step = 1, sep = "", width = "100%",
          animate = animationOptions(interval = 1000, loop = TRUE)
        )
      ),
      
      # ---- 1. Map ----
      div(
        id = "cs-map",
        class = "home-section jump-target cs-jump",
        h3("Malaria Across Europe"),
        p(
          class = "home-lead",
          "Estimated malaria cases by country. Azerbaijan is highlighted, and ",
          "hovering over a country shows its estimated case count."
        ),
        div(
          class = "home-map-frame",
          leafletOutput("eu_map", height = "calc(100vh - 340px)", width = "100%")
        ),
        p(
          class = "home-note",
          "Dark gray indicates no estimate available for that country and year."
        ),
        back_link("cs-top")
      ),
      
      # ---- 2. Azerbaijan ----
      div(
        id = "cs-az",
        class = "home-section jump-target cs-jump",
        h3("Focus: Azerbaijan"),
        p(
          class = "home-lead",
          "Malaria elimination involved sustained public health efforts, ",
          "including surveillance, prevention, diagnosis, and treatment. ",
          "Azerbaijan provides an example of progress toward eliminating ",
          "indigenous malaria transmission."
        ),
        
        fluidRow(
          column(width = 4, uiOutput("az_summary")),
          column(width = 8, plotOutput("az_trend", height = "380px"))
        ),
        
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
        ),
        back_link("cs-top")
      ),
      
      # ---- 3. Cases over time ----
      div(
        id = "cs-cases",
        class = "home-section jump-target cs-jump",
        h3("Malaria Cases Over Time"),
        p(
          class = "home-lead",
          "Compare the available global malaria estimates with estimates from ",
          "the WHO European Region. The two graphs use separate vertical ",
          "scales so the trend within each group is easier to see."
        ),
        div(
          class = "cs-two-col",
          div(
            class = "cs-panel",
            h4("Available Global Estimates"),
            helpText("Combined estimates from the countries included in the global dataset. This is not a complete global total."),
            plotOutput("cs_global_trend", height = "380px")
          ),
          div(
            class = "cs-panel",
            h4("WHO European Region"),
            helpText("Combined estimated malaria cases across countries in the WHO European Region."),
            plotOutput("cs_europe_trend", height = "380px")
          )
        ),
        p(
          class = "home-note",
          "About the data: the visualizations use WHO estimated malaria case data. ",
          "The available-country comparison sums estimates for the countries included ",
          "in the global dataset, rather than representing a complete global total."
        ),
        back_link("cs-top")
      ),
      
      # ---- 4. Healthcare resources ----
      div(
        id = "cs-resources",
        class = "home-section jump-target cs-jump",
        h3("Healthcare Resources and Malaria"),
        p(
          class = "home-lead",
          "This graph compares medical doctor availability with estimated ",
          "malaria cases per 100,000 people across WHO European Region ",
          "countries. Each point is one country in the selected year. ",
          "Azerbaijan is highlighted in pink, and the trend line shows the ",
          "overall relationship."
        ),
        plotOutput("resource_scatter", height = "460px"),
        helpText("Only countries with matching malaria, doctor, and population data in the selected year are shown."),
        
        div(
          class = "cs-panel",
          h4("What does the relationship show?"),
          textOutput("resource_description"),
          br(),
          p(
            class = "home-note",
            "Important: doctors per 10,000 people measures healthcare workforce availability, not general healthcare access such as affordability, distance to resources, or how care is distributed. Malaria cases per 100,000 total population is a calculated comparison measure, not the WHO malaria incidence rate per 100,000 people at risk. Further information is necessary to establish a causal relationship."
          )
        ),
        back_link("cs-top")
      ),
      
      # ---- Where next ----
      div(
        class = "home-section",
        mal_heading("Keep Exploring", "Where to go next."),
        mal_cards(
          mal_card("\U0001F465", "Who is most affected",
                   "Poverty, age, and access to care shape who gets sick.",
                   nav = "What is Malaria?", tab = "Who Is Most Affected",
                   hint = "Open \u2192"),
          mal_card("\U0001F48A", "Treatment options",
                   "How malaria is treated today.",
                   nav = "Treatment", tab = "Treatment Options",
                   hint = "Open \u2192"),
          mal_card("\U0001F3E0", "Back to the world map",
                   "Global incidence from 2000 to 2024.",
                   nav = "Home Page", hint = "Open \u2192")
        )
      ),
      
      div(
        class = "home-footer",
        "Data: WHO estimated malaria cases and medical doctors per 10,000; population from Our World in Data."
      )
    )
  ),
  
  # ---------------- TREATMENT ----------------
  
  tabPanel(
    "Treatment",
    fluidPage(
      br(),
      tabsetPanel(
        id = "treatment_tabs",
        
        # ---- Treatment (overview text + CDC algorithm) ----
        tabPanel(
          "Treatment Options",
          br(),
          mal_page(
            
            jump_section(
              "tr-top",
              mal_hero("Treating the disease", "Treatment", "Malaria is treated with antimalarial drugs, most often artemisinin-based combination therapies, chosen by parasite species, severity, age, and pregnancy status.")
            ),
            
            mal_heading("Choose a Topic", "Select a card to open the corresponding explanation below."),
            mal_cards(
              mal_card("\U0001F48A", "Artemisinin combination therapies",
                       "The preferred treatment: a fast-acting drug plus a long-lasting partner.",
                       jump = "tr-act"),
              mal_card("\U0001F9EA", "Chloroquine",
                       "An older drug that has lost effectiveness against P. falciparum.",
                       jump = "tr-chloroquine"),
              mal_card("\U0001F30D", "WHO-recommended combinations",
                       "Six ACT pairings, chosen using local efficacy studies.",
                       jump = "tr-who"),
              mal_card("\U0001F4CB", "CDC treatment algorithm",
                       "A diagnosis and treatment flow chart for U.S. providers.",
                       jump = "tr-algorithm"),
              mal_card("\u26A0\uFE0F", "What stands in the way",
                       "Resistance, invasive mosquitoes, and tests that miss the parasite.",
                       tab = "Threats to Controlling Malaria", hint = "Open \u2192"),
              mal_card("\U0001F5FA\uFE0F", "Where drugs are failing",
                       "Explore WHO efficacy studies on the map.",
                       tab = "Resistance and Mutations", hint = "Open \u2192")
            ),
            
            mal_heading("Treatment at a Glance", "Start with the overview, then open any topic to learn more."),
            mal_detail(
              "tr-overview", "How is malaria treated?", open = TRUE,
              paras("Malaria treatment uses antimalarial medicines. The choice depends on the Plasmodium species, severity of illness, patient age, pregnancy status, and the location where infection was acquired.",
                    "For many uncomplicated infections, artemisinin-based combination therapies (ACTs) are preferred. Severe malaria requires urgent hospital-based treatment, generally with intravenous artesunate."),
              p(class = "mal-note", "Treatment information is educational; clinicians should follow current national and WHO guidance.")
            ),
            mal_heading("Explore Treatment Options", "Click a heading to expand the explanation."),
            mal_detail(
              "tr-act", "Artemisinin-based combination therapies (ACTs)",
              p("ACTs combine a fast-acting artemisinin derivative with a longer-lasting partner drug. The artemisinin component rapidly reduces parasites in red blood cells, while the partner drug helps clear remaining parasites and reduces the risk of treatment failure."),
              p("Examples include artemether-lumefantrine (Coartem) and artesunate-mefloquine."),
              p("Other antimalarials used in particular circumstances include atovaquone-proguanil, quinine combined with an appropriate second drug, and primaquine. These are not interchangeable; treatment selection depends on the infection and patient.")
            ),
            mal_detail(
              "tr-chloroquine", "Chloroquine and resistance",
              p("Chloroquine phosphate is an older antimalarial drug. Widespread resistance has limited its usefulness against P. falciparum in many regions, although it can still be appropriate for infections acquired in areas where chloroquine sensitivity is established."),
              p("Drug resistance is one reason treatment guidance depends on the location where malaria was acquired.")
            ),
            mal_detail(
              "tr-who", "WHO-recommended ACT combinations",
              p("WHO recommendations draw on therapeutic efficacy studies of local parasites. ACT options described in this project include:"),
              tags$ul(
                tags$li("Artemether-lumefantrine (AL)"),
                tags$li("Artesunate-amodiaquine"),
                tags$li("Artesunate-mefloquine"),
                tags$li("Dihydroartemisinin-piperaquine"),
                tags$li("Artesunate-sulfadoxine-pyrimethamine"),
                tags$li("Artesunate-pyronaridine (recommended by WHO in November 2022)")
              ),
              p("The appropriate regimen and duration depend on current guidance, species, local resistance, and clinical circumstances.")
            ),
            mal_heading("Clinical Guidance", "Open the diagram when you want to see the decision process."),
            mal_detail(
              "tr-algorithm", "CDC malaria diagnosis and treatment algorithm",
              p("This diagram summarizes the CDC approach to diagnosing and treating malaria in the United States."),
              fig_box("CDC Guidelines.png", caption = "SOURCE: CDC Malaria Diagnosis & Treatment in the U.S.", alt = "CDC algorithm for diagnosing and treating malaria"),
              helpText("This page is general information, not medical advice. Treatment should follow a clinician and current national guidance.")
            ),
            back_link("tr-top"),
            
            mal_next(
              mal_card("\u26A0\uFE0F", "Threats to controlling malaria",
                       "Four biological threats identified by WHO.",
                       tab = "Threats to Controlling Malaria", hint = "Open \u2192"),
              mal_card("\U0001F5FA\uFE0F", "Resistance and mutations",
                       "Where treatments are failing and the genetics behind it.",
                       tab = "Resistance and Mutations", hint = "Open \u2192"),
              mal_card("\U0001F326\uFE0F", "Climate & resistance",
                       "The background on how resistance evolves.",
                       nav = "What is Malaria?", tab = "Climate & Resistance", hint = "Open \u2192")
            )
          )
        ),
        
        # ---- Threats to controlling malaria ----
        tabPanel(
          "Threats to Controlling Malaria",
          br(),
          mal_page(
            
            jump_section(
              "th-top",
              mal_hero("What stands in the way", "Threats to Controlling Malaria", "Invasive mosquitoes, insecticide and drug resistance, and gene deletions that hide the parasite from tests.")
            ),
            
            mal_heading("The Four Threats", "Click a card to jump to each one."),
            mal_cards(
              mal_card("\U0001F99F", "Invasive vector species",
                       "Mosquitoes that adapt their behavior and resist insecticides.",
                       jump = "th-vectors"),
              mal_card("\U0001F9EA", "Insecticide resistance",
                       "Mosquitoes that survive nets and indoor spraying.",
                       jump = "th-insecticide"),
              mal_card("\U0001F48A", "Drug efficacy and resistance",
                       "Treatment failure and kelch13 markers in Asia and Africa.",
                       jump = "th-drug"),
              mal_card("\U0001F9EC", "pfhrp2/3 gene deletions",
                       "Parasites that rapid tests cannot see.",
                       jump = "th-gene"),
              mal_card("\U0001F5FA\uFE0F", "See it on the map",
                       "Explore WHO efficacy studies by drug and country.",
                       tab = "Resistance and Mutations", hint = "Open \u2192")
            ),
            
            fluidRow(
              column(
                width = 12,
                div(
                  class = "threat-full-width",
                  mal_heading("Why Control Remains Difficult", "Open each threat to see the explanation, supporting figures, and sources."),
                  
                  p("According to WHO, there are four major biological threats to controlling malaria: invasive vector species, vector insecticide resistance, antimalarial drug efficacy and resistance, and parasite pfhrp2/3 gene deletions."),
                  
                  mal_detail(
                    "th-vectors", "1. Invasive Vector Species",
                    p("The issue with invasive vector species is that they may adapt to changing environments by altering their behavior patterns. Additionally, they may also become insecticide-resistant when exposed to current vector control interventions (WHO)."),
                    p("It is recommended by WHO for malaria-endemic countries to engage in entomological surveillance systems to provide the critical information needed in designing and implementing vector control strategies. This includes understanding of vector species\u2019 breeding and resting habitats, behaviors, feeding preferences, and resistance status to prevent the spread (WHO)."),
                    fig_box("Invasive Vectors.png",
                            caption = "SOURCE: WHO World Malaria Report",
                            alt = "Invasive Anopheles vector species"),
                    back_link("th-top")
                  ),
                  
                  mal_detail(
                    "th-insecticide", "2. Vector Insecticide Resistance",
                    div(class = "mal-quote", "\u201cBetween 2010 and 2020, 88 countries, of which 85 are currently malaria-endemic, reported insecticide resistance data for Anopheles mosquitoes to WHO.\u201d"),
                    div(class = "mal-quote", "\u201cResistance has been found in vectors from all six WHO regions and to all four classes of insecticide that have been widely used in adult malaria vector control.\u201d"),
                    p("To control the spread of mosquitoes, vector control interventions heavily rely on large-scale use of insecticides such as insecticide-treated nets (ITNs) and indoor residual insecticide spraying. Resistance (due to mutation) is a major concern, particularly for pyrethroids, an insecticide class that is commercially available and often utilized (WHO). This has been detected greatly in Western Africa. Resistance to neonicotinoids and the pyrrole insecticide chlorfenapyr (more recently introduced insecticides) has not yet been confirmed and may be an alternative solution until resistance grows; however, this will be more costly."),
                    p("WHO recommends enhanced insecticide monitoring and its impact on vector control. It is recommended that all malaria-endemic countries conduct tests once a year on insecticides from all classes in use or planned to be used on all major vector species from the different eco-epidemiological zones. Especially as there are relatively few data in regions other than Africa, such as South-East Asia."),
                    fig_box("Vector_insecticide_resistance.png",
                            caption = "SOURCE: WHO World Malaria Report",
                            alt = "Vector insecticide resistance"),
                    back_link("th-top")
                  ),
                  
                  mal_detail(
                    "th-drug", "3. Antimalarial Drug Efficacy and Resistance",
                    p("One of the biggest obstacles in the fight against malaria has been drug resistance. Drug efficacy is monitored through therapeutic efficacy studies (TES). This tracks clinical and parasitological outcomes among patients receiving antimalarial treatment. High treatment failure rates were detected with dihydroartemisinin-piperaquine (DHA-PPQ) in the Western Pacific Region, such as in Cambodia, the Lao People\u2019s Democratic Republic, and Viet Nam."),
                    fig_box("Treatment_failure_Southeastasia.png",
                            caption = "SOURCE: WHO World Malaria Report",
                            alt = "Treatment failure in South-East Asia"),
                    p("These studies are highly recommended as they promote changes in the use of medicine to ensure patients receive effective treatment."),
                    p("Artemisinin partial resistance is monitored using an established list of validated and candidate PfKelch13 markers associated with delayed clearance after a treatment containing artemisinin. These PfKelch13 mutations have emerged consistently in Eritrea, Rwanda and Uganda. However, there is an effective partner drug, resulting in a treatment failure rate below 10%. Other areas in which this mutation is highly prevalent are Cambodia, the Lao People\u2019s Democratic Republic and Viet Nam."),
                    fig_box("marker_drug_resistance.png",
                            caption = "SOURCE: WHO World Malaria Report",
                            alt = "Prevalence of PfKelch13 resistance markers"),
                    back_link("th-top")
                  ),
                  
                  mal_detail(
                    "th-gene", "4. Parasite pfhrp2/3 Gene Deletions",
                    p("The malaria parasite may have gene deletions in important markers for detection. This leads to false-negative diagnostic test results (RDTs). The rapid diagnostic test used to diagnose",
                      em("Plasmodium falciparum"),
                      "heavily relies on RDTs that target an antigen called histidine-rich protein 2 (HRP2), as well as some cross-reactivity with HRP3, a structurally similar parasite protein. It has been found that",
                      em("P. falciparum"),
                      "parasites lack the pfhrp2 gene that the RDTs target. This was first identified in Peru in 2010."),
                    fig_box("gene_deletions.png",
                            caption = "SOURCE: WHO World Malaria Report",
                            alt = "pfhrp2/3 gene deletions"),
                    back_link("th-top")
                  )
                )
              )
            ),
            
            mal_next(
              mal_card("\U0001F5FA\uFE0F", "Resistance and mutations",
                       "Where treatments are failing and the genetics behind it.",
                       tab = "Resistance and Mutations", hint = "Open \u2192"),
              mal_card("\U0001F48A", "Treatment options",
                       "The drugs and guidelines currently in use.",
                       tab = "Treatment Options", hint = "Open \u2192"),
              mal_card("\U0001F52C", "How malaria is diagnosed",
                       "Microscopy, rapid tests, and PCR.",
                       nav = "What is Malaria?", tab = "Symptoms & Clinical Presentations", hint = "Open \u2192")
            )
          )
        ),
        
        # ---- Resistance and Mutations: filters + interactive map ----
        tabPanel(
          "Resistance and Mutations",
          br(),
          mal_page(
            
            jump_section(
              "res-top",
              mal_hero("Tracking drug failure", "Resistance and Mutations", "Explore WHO therapeutic efficacy studies on the map and see how genetic markers relate to treatment failure.")
            ),
            
            mal_heading("On This Page", "Click a card to jump to a section."),
            mal_cards(
              mal_card("\U0001F5FA\uFE0F", "Interactive map",
                       "Filter WHO therapeutic efficacy records by drug, country, and year.",
                       jump = "res-map"),
              mal_card("\U0001F4CA", "Findings from WHO studies",
                       "Failure by drug, and day-3 positivity as an early warning.",
                       jump = "res-findings"),
              mal_card("\U0001F9EC", "Genetic markers (Pf8)",
                       "What the parasite genome says about resistance.",
                       jump = "res-markers"),
              mal_card("\U0001F30F", "Southeast Asia",
                       "Where kelch13 mutations are most common and how they spread.",
                       jump = "res-sea"),
              mal_card("\U0001F5C2\uFE0F", "Dataset overview",
                       "What is inside the WHO database, and a drug summary.",
                       tab = "Dataset overview", hint = "Open \u2192")
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
                  "measure", "Outcome shown on the map",
                  choices = NULL
                ),
                sliderInput(
                  "cap", "Color scale maximum (%)",
                  min = 10, max = 100, value = 50, step = 5
                ),
                hr(),
                textOutput("n_shown"),
                helpText(
                  "Records with a missing start year are excluded. ",
                  "Each record is a row in the WHO database, not necessarily a separate study."
                )
              ),
              
              mainPanel(
                width = 9,
                
                # Map, in the same bordered frame as the other pages
                jump_section(
                  "res-map",
                  div(
                    class = "home-map-frame",
                    leafletOutput("leaf", height = 600)
                  ),
                  p(
                    class = "home-note",
                    "Click any dot to see its country, study site, year, drug, species, and value."
                  ),
                  mal_detail(
                    "res-how", "How to use the interactive map",
                    p("Each dot represents a WHO therapeutic efficacy study database record at its study site."),
                    tags$ul(
                      tags$li(strong("Choose an outcome: "), "Switch between per-protocol treatment failure, Kaplan-Meier treatment failure, and day-3 positivity."),
                      tags$li(strong("Read the colors: "), "Darker purple indicates lower values and bright yellow indicates higher values. Adjust the color scale maximum with the slider."),
                      tags$li(strong("Filter records: "), "Select species, drugs, countries, and study years. Leave Drug and Country empty to include all."),
                      tags$li(strong("Explore: "), "Zoom, drag, and click a dot for site, year, drug, species, and outcome details.")
                    ),
                    p(strong("Interpreting the map: "), "Most studies found treatment failure below 10%. Higher values cluster in parts of Southeast Asia, with scattered higher values in Africa. A dot represents a record, not necessarily an entire study.")
                  )
                ),
                
                br(),
                div(
                  class = "resistance-content",
                  
                  mal_heading("Explore the Evidence", "Open a topic below to read the research and view its figures."),
                  mal_detail(
                    "res-context", "Understanding resistance and the WHO / MalariaGEN datasets", open = TRUE,
                    p("Constant shifts in land use, climate, migration, and drug and insecticide selection pressures alter mutation rates and susceptibility to malaria. The genomics of parasites, mosquitoes, and humans can aid in observing these patterns of the disease to understand why some people are more resistant to malaria and what factors are mutating the parasite. For example, scientists can identify when and where drugs are not effective due to resistance and use this information to find new targets for drugs, vaccines, and insecticides (MalariaGen)."),
                    
                    p("MalariaGen is a public genomic database acting as a key surveillance system. When considering transmission and the evolving chain of human, parasite, and mosquito, it is important to monitor locations over years, specifically the trends of these changes. This database can be compared to WHO\u2019s efficacy data to further analyze resistance."),
                    
                    p("The following table shows the drugs we investigated to identify resistance and their markers."),
                    div(class = "ds-panel", tableOutput("drug_marker_table")),
                    
                    h4("Key terms"),
                    div(class = "ds-panel", tableOutput("key_terms_table")),
                    
                    p("We used the MalariaGEN Pf8 dataset, which holds genetic data on P. falciparum parasites from patients around the world. After quality control, 24,409 genotyped samples from 33 countries, collected between 1966 and 2022, were available. Most came from West Africa (8,914), eastern Southeast Asia (5,880) and East Africa (4,173)."),
                    
                    fig_box(
                      "WHO Therapeutic efficacy studies by year.png",
                      tag = em("P. falciparum"),
                      label = "Fig 1.",
                      caption = "The WHO database holds roughly 160\u2013195 records per year from 2010 to 2018, falling to under 50 a year after 2020, with very few from 2023\u20132024. The recent decline may reflect reporting delays rather than fewer studies, so recent trends should be read cautiously. This may also reflect geopolitical influences. A \u201crecord\u201d is a row in the database, usually one treatment arm at one site, so the bars do not count separate studies.",
                      alt = "Number of WHO therapeutic efficacy study records by year"
                    )
                  ),
                  
                  mal_detail(
                    "res-findings", "Findings from WHO therapeutic efficacy studies",
                    fig_box(
                      "failure_by_drug.png",
                      label = "Fig 2.",
                      caption = "The typical study found very low failure for every drug (medians near 0\u20133%), but the spread differs. Dihydroartemisinin-piperaquine has a long tail of studies above 10%, including several above 50%, consistent with the piperaquine resistance documented in Cambodia, Laos and Vietnam. Artemether-lumefantrine, artesunate-amodiaquine, artesunate-mefloquine and artesunate-pyronaridine each have a smaller number of studies above 10%. The pattern is not in the median, which is why the individual dots matter.",
                      alt = "Treatment failure by drug"
                    ),
                    fig_box(
                      "day3_vs_failure.png",
                      label = "Fig 3.",
                      caption = "Day-3 positivity and treatment failure show a weak positive relationship: the fitted line rises from near 0% failure at 0% day-3 positivity to roughly 25% at the highest values. The scatter is wide, though. Many studies with high day-3 positivity had little or no treatment failure, which fits the idea that a good partner drug can clear parasites that the artemisinin component clears slowly. Day-3 positivity is an early warning, not a reliable predictor of failure.",
                      alt = "Day-3 positivity versus treatment failure"
                    ),
                    back_link("res-map", "\u2191 Back to the map")
                  ),
                  
                  mal_detail(
                    "res-markers", "Genetic markers of resistance (MalariaGEN Pf8)",
                    p("The WHO data show how well drugs work in patients. The Pf8 data show the genetic changes in parasites that are linked to resistance. Prevalence below means the share of samples carrying a mutation; a sample with both a mutant and a normal version of a gene (a mixed infection) was counted as carrying the mutation. Estimates based on fewer than 10 samples are not shown.")
                  ),
                  
                  mal_detail(
                    "res-regions", "Resistance differs sharply by region",
                    fig_box(
                      "population_marker_heatmap.png",
                      label = "Fig 4.",
                      caption = "Prevalence of resistance markers in each Pf8 population. Each tile is one marker in one region, and the number in the tile is the percent of samples carrying it. Three patterns stand out. Mutations in kelch13, the main marker of artemisinin partial resistance, are common in Southeast Asia (62% of samples in eastern Southeast Asia, 24% in western Southeast Asia) and essentially absent in the African populations. The pfcrt K76T mutation linked to chloroquine resistance is near universal in Asia (94-100%) but lower in Africa (19% in East Africa, 31% in West Africa, 44% in Central Africa). The dhps K540E mutation, part of the pattern linked to sulfadoxine-pyrimethamine resistance, is found in 86% of East African samples but only 1% of West African samples.",
                      alt = "Heatmap of resistance marker prevalence in each Pf8 population"
                    )
                  ),
                  
                  mal_detail(
                    "res-sea", "Artemisinin partial resistance in Southeast Asia",
                    fig_box(
                      "map_admin1_k13_validated_southeast_asia.png",
                      label = "Fig 5.",
                      caption = "Share of samples with a WHO-validated kelch13 mutation in each province (all years combined). Each dot is one province; color shows prevalence and size shows the number of samples. Prevalence is highest in Vietnam (79% of samples) and Cambodia (68%), followed by Thailand (30%), Laos (29%) and Myanmar (27%). The C580Y mutation accounts for most of this in Vietnam (72%) and Cambodia (61%), but only 3% of samples in Myanmar, so different kelch13 mutations are circulating there. Dots are placed at each province's center point, not at the exact collection site.",
                      alt = "Map of Southeast Asia showing kelch13 mutation prevalence by province"
                    ),
                    fig_box(
                      "k13_validated_heatmap_country_year.png",
                      label = "Fig 6.",
                      caption = "Prevalence of validated kelch13 mutations by country and year, for countries with at least one detection. The mutations have spread over time. In Vietnam, prevalence rose from 43% (2010-2012) to 93% (2019-2022). In Cambodia it rose from 51% (2010-2012) to 88% (2016-2018), and in Laos from 2% (2010-2012) to 53% (2019-2022). Thailand went from 4% (2009 or earlier) to 99% (2016-2018), though the latter figure rests on only 72 samples. Myanmar stayed roughly flat at about 26%. Across all of Africa in Pf8, only two samples carry a validated kelch13 mutation (C469Y, one from Ghana and one from Nigeria), and none are from East Africa. Because WHO reports these mutations in Eritrea, Rwanda and Uganda, this most likely reflects which countries and years Pf8 covers (it ends in 2022), not an absence of the problem.",
                      alt = "Heatmap of kelch13 mutation prevalence by country and year"
                    )
                  ),
                  
                  mal_detail(
                    "res-older", "Older drugs: chloroquine and sulfadoxine-pyrimethamine",
                    fig_box(
                      "trend_crt_K76T_clean.png",
                      label = "Fig 7.",
                      caption = "Prevalence of pfcrt K76T over time in each Pf8 population. Across Africa, K76T fell from 81% of samples in the 1990s to 18% in 2016-2022. This is consistent with chloroquine being replaced by artemisinin-based combinations: when the drug stopped being used, parasites without the mutation became common again. In Asia and Oceania the mutation stayed near 100%.",
                      alt = "Line chart of pfcrt K76T prevalence over time by population"
                    ),
                    fig_box(
                      "trend_dhps_K540E_east_africa.png",
                      label = "Fig 8.",
                      caption = "Prevalence of dhps K540E over time in East African countries. The mutation has become very common: in Kenya it rose from 48% of samples (2005 or earlier) to 98% (2016-2022), and it is above 90% in Malawi and Tanzania. The full pattern of five mutations (three in dhfr, two in dhps) linked to the greatest loss of sulfadoxine-pyrimethamine effectiveness was found in 77% of East African samples, versus 1% in West Africa.",
                      alt = "Line chart of dhps K540E prevalence over time in East African countries"
                    )
                  ),
                  
                  mal_detail(
                    "res-limits", "What the markers can and cannot tell us",
                    tags$ul(
                      tags$li("Pf8 samples are not the patients enrolled in the WHO efficacy studies, so any comparison between the two datasets is by country and time period only. It can show association, not that a marker caused a treatment failure."),
                      tags$li("A mutation can be common without treatment failing, especially when the partner drug is still effective. This is why kelch13 prevalence and failure rates can disagree."),
                      tags$li("Sampling is uneven across countries and years, and results for small groups are less reliable. A missing country does not mean no resistance."),
                      tags$li("Prevalence figures combine all years unless stated, which can hide recent changes.")
                    ),
                    back_link("res-top")
                  )
                )
              )
            )
          )
        ),
        
        # ---- Dataset overview ----
        tabPanel(
          "Dataset overview",
          br(),
          mal_page(
            
            jump_section(
              "ds-top",
              mal_hero("Behind the data", "Dataset Overview", "What is inside the WHO therapeutic efficacy database, plus a summary of the records that match your filters.")
            ),
            
            # Headline numbers (filled in by the server)
            mal_stats(
              mal_stat(textOutput("ds_records", inline = TRUE), "records in the WHO database"),
              mal_stat(textOutput("ds_countries", inline = TRUE), "countries"),
              mal_stat(textOutput("ds_species", inline = TRUE), "parasite species"),
              mal_stat(textOutput("ds_drugs", inline = TRUE), "drugs tested"),
              mal_stat(textOutput("ds_years", inline = TRUE), "study start years")
            ),
            
            mal_heading("In This Section", "Click a card to jump to a table or chart."),
            mal_cards(
              mal_card("\U0001F9EC", "Parasite species",
                       "How many records exist for each Plasmodium species.",
                       jump = "ds-species-section"),
              mal_card("\U0001F48A", "Most common drugs",
                       "Which drugs appear most often in the database.",
                       jump = "ds-drugs-section"),
              mal_card("\U0001F4CB", "Drug summary",
                       "Median failure and day-3 positivity for your current filters.",
                       jump = "ds-summary-section"),
              mal_card("\U0001F5FA\uFE0F", "Back to the map",
                       "Change the filters on the Resistance and Mutations tab.",
                       tab = "Resistance and Mutations", hint = "Open \u2192")
            ),
            
            # ---- Species ----
            jump_section(
              "ds-species-section",
              mal_heading("Parasite Species", "All records in the database, before any filters."),
              div(
                class = "ds-grid",
                div(
                  class = "ds-panel",
                  h4("Records by species"),
                  plotOutput("ds_species_plot", height = "320px")
                ),
                div(
                  class = "ds-panel",
                  h4("Species table"),
                  tableOutput("species_table")
                )
              ),
              back_link("ds-top")
            ),
            
            # ---- Drugs ----
            jump_section(
              "ds-drugs-section",
              mal_heading("Most Common Drugs", "The ten drugs with the most records, and the 30 most common in full."),
              div(
                class = "ds-grid",
                div(
                  class = "ds-panel",
                  h4("Top 10 drugs"),
                  plotOutput("ds_drug_plot", height = "400px")
                ),
                div(
                  class = "ds-panel",
                  h4("Drug table"),
                  div(class = "ds-scroll", tableOutput("drug_table"))
                )
              ),
              back_link("ds-top")
            ),
            
            # ---- Drug summary (uses the filters) ----
            jump_section(
              "ds-summary-section",
              mal_heading("Drug Summary", "Uses the filters on the Resistance and Mutations tab."),
              div(
                class = "ds-panel",
                div(class = "ds-scroll", tableOutput("drug_summary")),
                br(),
                downloadButton("dl_summary", "Download CSV")
              ),
              back_link("ds-top")
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
      br(),
      mal_page(
        jump_section(
          "fr-top",
          mal_hero("What the evidence tells us", "Final Remarks",
                   "What we learned about malaria, why control remains difficult, and where research can go next.")
        ),
        mal_heading("On This Page", "Explore our conclusions, future directions, and sources."),
        mal_cards(
          mal_card("\U0001F30D", "Concluding Remarks",
                   "The central findings from our malaria research.", jump = "fr-conclusion"),
          mal_card("\U0001F52D", "Additional Avenues",
                   "Questions and approaches for future malaria research.", jump = "fr-avenues"),
          mal_card("\U0001F4D6", "References",
                   "Sources used throughout the website.", jump = "fr-references"),
          mal_card("\U0001F4DA", "Further Reading",
                   "Explore more research and public-health resources.", jump = "fr-further")
        ),
        jump_section(
          "fr-conclusion",
          mal_heading("Concluding Remarks",
                      "Malaria is preventable and treatable, but controlling it requires more than any single intervention."),
          paras(
            "Across this website, we examined how malaria is transmitted, how it has changed geographically and historically, and why it continues to threaten public health. Global surveillance shows that malaria remains a major cause of illness and death, with the heaviest burden concentrated in regions where exposure, healthcare access, and resources are unequal.",
            "The treatment and resistance evidence reveals a second challenge: the tools used to fight malaria must continually adapt. Artemisinin-based combination therapies remain central to treatment, yet delayed parasite clearance, treatment failures, and resistance-associated genetic variants make ongoing monitoring essential. Likewise, insecticide resistance and changing mosquito distributions can undermine established prevention strategies.",
            "The interactive maps and figures help illustrate how uneven these challenges are across countries and over time. However, a genetic marker is not the same as confirmed clinical treatment failure, and surveillance gaps limit what can be concluded from any one dataset. Combining molecular surveillance with clinical efficacy studies, vector monitoring, and local health-system information provides a more complete picture.",
            "Ultimately, progress against malaria depends on sustained investment in prevention, early diagnosis, effective treatment, surveillance, and equitable access to care. Understanding both the biology of the parasite and the social and environmental conditions surrounding transmission is essential for lasting malaria control."
          ),
          back_link("fr-top")
        ),
        hr(),
        jump_section(
          "fr-avenues",
          mal_heading("Additional Avenues",
                      "Our findings raise important questions about what malaria control should prioritize next."),
          p(class = "mal-lead",
            "The challenges identified throughout this project point toward several complementary directions for future research and public-health action."),
          mal_detail("fr-next-genomics", "1. Expand genomic and treatment-efficacy surveillance", open = TRUE,
                     paras("Tracking resistance-associated variants alongside therapeutic efficacy studies can help identify emerging threats before they become widespread. Future work could examine how changes in marker frequencies correspond to clinical outcomes, while accounting for uneven sampling and differences between countries.")),
          mal_detail("fr-next-vectors", "2. Monitor changing mosquito populations and insecticide resistance",
                     paras("Vector surveillance is essential as mosquito distributions and insecticide susceptibility change. Research could compare the effectiveness of different insecticide-treated nets, investigate invasive Anopheles species, and evaluate how local environmental conditions affect mosquito control.")),
          mal_detail("fr-next-equity", "3. Address barriers to prevention, diagnosis, and treatment",
                     paras("Malaria risk is shaped by more than parasite biology. Future studies could examine how housing, distance from clinics, costs of care, and access to diagnostic tests and prevention tools influence outcomes, especially in communities experiencing persistent malaria transmission.")),
          mal_detail("fr-next-climate", "4. Investigate climate and environmental change",
                     paras("Temperature, rainfall, land use, and other environmental conditions can affect mosquito habitats and transmission. Linking environmental observations with surveillance data could help identify where risk may shift, without assuming that climate change has the same effect in every region.")),
          mal_detail("fr-next-tools", "5. Evaluate new and combined prevention strategies",
                     paras("Vaccines, effective medicines, improved diagnostics, and vector-control interventions each contribute to malaria prevention and care. Research comparing combined approaches could clarify which strategies are most effective and practical in different local settings.")),
          p(class = "mal-lead",
            "Together, these avenues emphasize the need to connect molecular and clinical research with environmental monitoring and community-level public health. The resources below document the evidence used in this website and offer starting points for further investigation."),
          back_link("fr-top")
        ),
        hr(),
        p(class = "mal-lead",
          "This website draws on scientific research, global health surveillance, and guidance from WHO, CDC, and MalariaGEN. References lists material supporting the website; Further Reading offers additional resources for deeper exploration."),
        jump_section(
          "fr-references",
          mal_heading("References",
                      "Sources supporting the website's scientific explanations, historical information, and data visualizations. Expand a category to see its sources."),
          reference_group("ref-overview", "1. General malaria information and epidemiology", open = TRUE,
                          reference_entry("World Health Organization. (2025). World malaria report 2025.",
                                          "https://www.who.int/teams/global-malaria-programme/reports/world-malaria-report-2025/",
                                          "Global cases, deaths, trends, and progress toward malaria control."),
                          reference_entry("World Health Organization. Malaria (fact sheet).",
                                          "https://www.who.int/news-room/fact-sheets/detail/malaria",
                                          "Overview of disease burden, symptoms, prevention, and treatment."),
                          reference_entry("Centers for Disease Control and Prevention. Malaria.",
                                          "https://www.cdc.gov/malaria/",
                                          "CDC's general malaria information and prevention resources."),
                          reference_entry("World Health Organization. Global Malaria Observatory.",
                                          "https://www.who.int/data/gho/data/themes/malaria/GHO/malaria",
                                          "Country-level indicators and surveillance data.")
          ),
          reference_group("ref-biology", "2. Biology, transmission, and diagnosis",
                          reference_entry("Centers for Disease Control and Prevention. DPDx: Malaria.",
                                          "https://www.cdc.gov/dpdx/malaria/index.html",
                                          "Parasite life cycle, species identification, and laboratory diagnosis."),
                          reference_entry("Centers for Disease Control and Prevention. Causes of malaria.",
                                          "https://www.cdc.gov/malaria/causes/",
                                          "Transmission and Plasmodium parasites."),
                          reference_entry("Centers for Disease Control and Prevention. Malaria diagnostic tests.",
                                          "https://www.cdc.gov/malaria/hcp/diagnosis-testing/malaria-diagnostic-tests.html",
                                          "Microscopy, rapid diagnostic tests, and other diagnostic approaches."),
                          reference_entry("Centers for Disease Control and Prevention. Evaluation and diagnosis of malaria.",
                                          "https://www.cdc.gov/malaria/hcp/clinical-guidance/evaluation-and-diagnosis.html",
                                          "Clinical evaluation and diagnostic guidance.")
          ),
          reference_group("ref-treatment", "3. Treatment, prevention, and vaccines",
                          reference_entry("World Health Organization. WHO guidelines for malaria.",
                                          "https://www.who.int/publications/i/item/guidelines-for-malaria",
                                          "Recommendations for malaria prevention, diagnosis, and treatment."),
                          reference_entry("Centers for Disease Control and Prevention. Appendix B: Malaria diagnosis and treatment algorithm.",
                                          "https://www.cdc.gov/malaria/hcp/clinical-guidance/appendix-b-algorithm.html",
                                          "Clinical treatment decision aid; also linked on the Treatment page."),
                          reference_entry("World Health Organization. Malaria vaccines (RTS,S and R21).",
                                          "https://www.who.int/teams/global-malaria-programme/vaccines",
                                          "Vaccine recommendations and implementation information."),
                          reference_entry("World Health Organization. (2024). Malaria vaccine: WHO position paper – May 2024.",
                                          "https://www.who.int/publications/i/item/who-wer-9919-225-248",
                                          "Evidence and recommendations concerning malaria vaccines.")
          ),
          reference_group("ref-genetics", "4. Antimalarial drug resistance and mutations",
                          reference_entry("World Health Organization. Compendium of molecular markers for antimalarial drug resistance.",
                                          "https://www.who.int/tools/compendium-of-molecular-markers-for-antimalarial-drug-resistance",
                                          "Classification of genetic markers associated with antimalarial resistance."),
                          reference_entry("MalariaGEN. P. falciparum Community Project.",
                                          "https://www.malariagen.net/project/p-falciparum-community-project/",
                                          "Genomic surveillance and population-level parasite variation."),
                          reference_entry("MalariaGEN. Pf7: Open dataset of Plasmodium falciparum genome variation.",
                                          "https://malariagen.net/data_package/open-dataset-plasmodium-falciparum-v70/",
                                          "Dataset resource; cite the precise release used for each map or chart."),
                          reference_entry("MalariaGEN consortium. (2023). Pf7: An open dataset of Plasmodium falciparum genome variation in 20,000 worldwide samples.",
                                          "https://pubmed.ncbi.nlm.nih.gov/36864926/",
                                          "Primary dataset publication describing global genomic sampling."),
                          reference_entry("World Health Organization. Malaria threats map.",
                                          "https://www.who.int/teams/global-malaria-programme/surveillance/malaria-threats-map",
                                          "Surveillance of treatment efficacy, resistance, and vector threats.")
          ),
          reference_group("ref-vectors", "5. Mosquito vectors and insecticide resistance",
                          reference_entry("World Health Organization. (2017). Global vector control response 2017–2030.",
                                          "https://www.who.int/publications/i/item/9789241512978",
                                          "Integrated vector management, surveillance, and insecticide-resistance response."),
                          reference_entry("World Health Organization. Malaria threats map.",
                                          "https://www.who.int/teams/global-malaria-programme/surveillance/malaria-threats-map",
                                          "Vector resistance and invasive mosquito surveillance."),
                          reference_entry("World Health Organization. (2025). World malaria report 2025.",
                                          "https://www.who.int/teams/global-malaria-programme/reports/world-malaria-report-2025/",
                                          "Current evidence on vector control and resistance challenges.")
          ),
          reference_group("ref-history", "6. History and malaria elimination",
                          reference_entry("de Zulueta, J. (1987). Changes in the geographical distribution of malaria throughout history. Parassitologia, 29(2–3), 193–205.",
                                          "https://pubmed.ncbi.nlm.nih.gov/3334082/",
                                          "Historical changes in the global distribution of malaria."),
                          reference_entry("Centers for Disease Control and Prevention. History of malaria.",
                                          "https://www.cdc.gov/malaria/history/",
                                          "History of malaria control and elimination in the United States.")
          ),
          reference_group("ref-data", "7. Data sources and interactive visualizations",
                          reference_entry("Our World in Data. Malaria.",
                                          "https://ourworldindata.org/malaria",
                                          "Global malaria trends and downloadable indicators."),
                          reference_entry("World Health Organization. Global Malaria Observatory.",
                                          "https://www.who.int/data/gho/data/themes/malaria/GHO/malaria",
                                          "WHO indicators used in country and regional comparisons."),
                          reference_entry("MalariaGEN. Pf7 open dataset.",
                                          "https://malariagen.net/data_package/open-dataset-plasmodium-falciparum-v70/",
                                          "Genomic data supporting resistance-marker exploration, if this release was used.")
          ),
          p(class = "mal-note", "Check each graph and image against its original source. Cite the exact dataset version and figure attribution wherever applicable. Some entries are abbreviated and should be completed for a formal APA bibliography."),
          back_link("fr-top")
        ),
        hr(),
        jump_section(
          "fr-further",
          mal_heading("Further Reading",
                      "Optional publications and research resources for exploring malaria beyond the material presented on this website."),
          reference_group("ref-social", "Climate, poverty, and public health",
                          reference_entry("Worrall, E., Basu, S., & Hanson, K. (2005). Is malaria a disease of poverty? A review of the literature. Tropical Medicine & International Health, 10(10), 1047–1059.",
                                          "https://pubmed.ncbi.nlm.nih.gov/16185240/",
                                          "Relationship between socioeconomic conditions, malaria risk, and control."),
                          reference_entry("Cardona-Arias et al. (2019). Social determination and determinants of malaria: A systematic review, 1980–2018.",
                                          "https://journal.paho.org/en/articles/social-determination-and-determinants-malaria-systematic-review-1980-2018",
                                          "Structural, social, and environmental determinants of malaria."),
                          reference_entry("Social determinants of malaria in low- and middle-income countries: A mixed-methods systematic review. (2025).",
                                          "https://pubmed.ncbi.nlm.nih.gov/40420124/",
                                          "Housing, income, livelihoods, and healthcare access; confirm full bibliographic metadata.")
          ),
          reference_group("reading-emerging", "Emerging antimalarial drug resistance",
                          reference_entry("Spatio-temporal trends of Plasmodium falciparum antimalarial drug resistance markers across sub-Saharan Africa, 2010–2024. (2026).",
                                          "https://pmc.ncbi.nlm.nih.gov/articles/PMC13545778/",
                                          "Research on changing resistance-marker prevalence across regions; verify full bibliographic details."),
                          reference_entry("Mapping Plasmodium falciparum mutations in Africa: A critical review of emerging drug resistance and implications for malaria control. (2025).",
                                          "https://pmc.ncbi.nlm.nih.gov/articles/PMC12485076/",
                                          "Additional review of molecular resistance markers and geographic trends.")
          ),
          reference_group("reading-research", "Research databases and additional resources",
                          reference_entry("MalariaGEN. About MalariaGEN.",
                                          "https://malariagen.net/about/",
                                          "Learn about the research network and its genomic surveillance work."),
                          reference_entry("National Library of Medicine. PubMed.",
                                          "https://pubmed.ncbi.nlm.nih.gov/",
                                          "Search the biomedical literature for new malaria research."),
                          reference_entry("World Health Organization. Malaria research and surveillance programmes.",
                                          "https://www.who.int/teams/global-malaria-programme",
                                          "Explore malaria policy, surveillance, and ongoing public-health initiatives.")
          ),
          back_link("fr-top")
        ),
      )
    )
  ),
  
  # ---------------- ABOUT US ----------------
  
  tabPanel(
    "About Us",
    fluidPage(
      br(),
      h2("Meet the Team!"),
      p("The contributors behind this malaria research project."),
      
      # Hover animation for contributor bubbles
      tags$style(HTML("
      .team-card {
        background-color: #21113D;
        border: 2px solid #63318E;
        border-radius: 20px;
        padding: 22px;
        margin: 10px 0;
        text-align: left;
        min-height: 450px;
        transition: transform 0.25s ease, box-shadow 0.25s ease;
      }

      .team-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 10px 25px rgba(177, 54, 160, 0.35);
      }

      .team-card img {
        width: 180px;
        height: 180px;
        object-fit: cover;
        border-radius: 50%;
        display: block;
        margin: 0 0 18px 0;
      }

      .team-card h3 {
        text-align: left;
        color: #FF3CAC;
      }

      .team-card p {
        text-align: left;
        color: #F5F5F7;
      }

      .team-card .major {
        font-size: 16px;
        font-weight: bold;
        color: #B9B1C9;
        margin-top: -5px;
        margin-bottom: 12px;
      }
    ")),
      
      fluidRow(
        
        column(
          width = 4,
          div(
            class = "team-card",
            tags$img(src = "About Us Tab/lex.png"),
            h3("Alexis Thompson"),
            p(
              "B.S. Neuroscience and Minor in Poverty and Human Capabilities | Pre-medicine",
              class = "major"
            ),
            p("Hi! I am from Glassboro, NJ. I enjoy living in the Blue Ridge Mountains because I love backpacking and the outdoors!")
          )
        ),
        
        column(
          width = 4,
          div(
            class = "team-card",
            tags$img(src = "About Us Tab/brianna_headshot.PNG"),
            h3("Brianna Stone"),
            p(
              "B.S. Neuroscience and B.A. in Philosophy with Honors | Pre-medicine",
              class = "major"
            ),
            p(
              "Hey! I am from ",
              em("Colorful Colorado"),
              ", in a valley in the San Juans. I love climbing and hiking, but I've never gone skiing!"
            )
          )
        ),
        
        column(
          width = 4,
          div(
            class = "team-card",
            tags$img(src = "About Us Tab/Becca_Headhsot.PNG"),
            h3("Rebecca Goldman"),
            p(
              "B.S. Biology and B.A. in Environmental Studies | Pre-medicine",
              class = "major"
            ),
            p("Hi! I am from Honey Brook, Pennsylvania. I love traveling the globe and camping. My favorite place I've been is Botswana, where I lived for 5 months!")
          )
        )
        
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
  
  # Finds a data file by name: first in the usual folders, then anywhere
  # below the folder the app was started from. Returns NULL if not found.
  find_data_file <- function(file_name,
                             folders = c(".", "Becca", "data", "..", "../Becca")) {
    
    for (f in folders) {
      path <- file.path(f, file_name)
      if (file.exists(path)) return(path)
    }
    
    # also search the folder above (for example when the script sits in a
    # "Versions" subfolder next to the data folder)
    everything <- c(
      list.files(".",  recursive = TRUE, full.names = TRUE),
      list.files("..", recursive = TRUE, full.names = TRUE)
    )
    hit <- everything[basename(everything) == file_name]
    
    if (length(hit) > 0) hit[1] else NULL
  }
  
  who_name  <- "MTM_THERAPEUTIC_EFFICACY_STUDY_20261012.xlsx"
  who_sheet <- "Data"
  who_xlsx  <- find_data_file(who_name)
  who_cache <- find_data_file("who_clean.rds")
  
  if (is.null(who_cache)) {
    who_cache <- file.path(
      if (!is.null(who_xlsx)) dirname(who_xlsx) else ".",
      "who_clean.rds"
    )
  }
  
  prepare_who <- function() {
    
    if (file.exists(who_cache)) return(readRDS(who_cache))
    
    if (is.null(who_xlsx)) {
      stop(
        "WHO Excel file not found. Looked for '", who_name, "' in ",
        getwd(), " and its subfolders. ",
        "Put the file in that folder or in a 'Becca' subfolder."
      )
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
  
  drug_marker_table <- data.frame(
    Drug = c("Artemisinins (base of all ACTs)",
             "Artemether-lumefantrine",
             "Chloroquine",
             "Sulfadoxine-pyrimethamine",
             "Piperaquine (partner in DHA-piperaquine)"),
    `Why it matters` = c("Partial resistance makes parasites clear more slowly",
                         "Widely used first-line ACT in Africa",
                         "Former standard, now largely failed for P. falciparum",
                         "Now mainly used for prevention",
                         "Resistance documented in Southeast Asia"),
    `Pf8 genetic markers` = c("kelch13 mutations",
                              "pfmdr1 N86 / 184F / 1246D (NFD), pfcrt K76 wild type",
                              "pfcrt K76T and the CVIET haplotype",
                              "dhfr triple mutant, dhps K540E, SP \u201cquintuple\u201d mutant",
                              "Plasmepsin 2/3 amplification"),
    `Compared with (WHO efficacy data)` = c("Day-3 positivity and treatment failure",
                                            "Artemether-lumefantrine treatment failure",
                                            "Chloroquine treatment failure",
                                            "SP treatment failure",
                                            "Mapped in Pf8 only; not linked to WHO failure"),
    check.names = FALSE
  )
  
  key_terms_table <- data.frame(
    Term = c("Treatment failure",
             "Day-3 positivity",
             "Partial artemisinin resistance",
             "10% failure threshold"),
    Meaning = c("The infection came back or never cleared; usually PCR-corrected to separate true failure from a new infection.",
                "Share of patients still showing parasites on day 3. High values are an early warning of artemisinin partial resistance.",
                "Slower parasite clearance. It often does not cause outright failure because the partner drug clears the rest, so kelch13 prevalence and treatment failure can disagree.",
                "The level at which WHO advises a country to reconsider its first-line treatment."),
    check.names = FALSE
  )
  
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
  
  # ---- Chart styling: every chart sits on a dark panel that matches the site ----
  
  panel_bg   <- "#2a2547"
  text_light <- "#f6d5ee"
  text_muted <- "#B193CC"
  
  chart_theme <- theme_minimal(base_size = 13) +
    theme(
      text             = element_text(color = text_light),
      plot.title       = element_text(face = "bold", size = 15, color = "#FFADE5"),
      plot.subtitle    = element_text(color = text_muted),
      plot.caption     = element_text(color = text_muted),
      axis.text        = element_text(color = "#e6c3dc"),
      axis.title       = element_text(color = "#e6c3dc"),
      panel.grid.major = element_line(color = "#FFADE526", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = panel_bg, color = NA),
      panel.background = element_rect(fill = panel_bg, color = NA),
      legend.position  = "bottom",
      legend.text      = element_text(color = text_light),
      legend.title     = element_text(color = text_light),
      legend.background = element_rect(fill = panel_bg, color = NA),
      legend.key       = element_rect(fill = NA, color = NA),
      plot.title.position = "plot",
      plot.margin      = margin(15, 20, 15, 15)
    )
  
  # Bright colors that read well on the dark panel
  line_palette <- c("#f187d0", "#FFD166", "#6ee7e0", "#9d8cff",
                    "#7fd17f", "#ff8c69", "#8ecae6", "#e5b3fe")
  
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
    leaflet(world, options = leafletOptions(minZoom = 1)) %>%
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
        title = paste0("Incidence per 1,000 at risk \u2014 ", input$year),
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
  
  # ---- Sneak peek charts on the home page (dark panel background) ----
  
  peek_theme <- theme_void(base_size = 12) +
    theme(
      axis.text.y = element_text(color = text_light, size = 10, hjust = 1),
      axis.text.x = element_text(color = text_muted, size = 9),
      plot.background = element_rect(fill = panel_bg, color = NA),
      panel.background = element_rect(fill = panel_bg, color = NA),
      plot.margin = margin(10, 14, 10, 10)
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
  }, bg = panel_bg)
  
  output$peek_trend <- renderPlot({
    
    d <- malaria_countries %>%
      group_by(year) %>%
      summarise(median = median(incidence), .groups = "drop")
    
    ggplot(d, aes(x = year, y = median)) +
      geom_area(fill = "#c2168c", alpha = 0.35) +
      geom_line(color = "#f187d0", linewidth = 1.3) +
      scale_x_continuous(breaks = range(d$year)) +
      peek_theme
  }, bg = panel_bg)
  
  output$peek_case <- renderPlot({
    
    cs_ready()
    
    d <- cs$europe_trend
    
    ggplot(d, aes(x = year, y = cases)) +
      geom_area(fill = "#c2168c", alpha = 0.35, na.rm = TRUE) +
      geom_line(color = "#f187d0", linewidth = 1.3, na.rm = TRUE) +
      scale_x_continuous(breaks = range(d$year)) +
      peek_theme
  }, bg = panel_bg)
  
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
  }, bg = panel_bg)
  
  # ---- Navigation from the home page ----
  
  observeEvent(input$go_malaria, {
    updateNavbarPage(session, "main_nav", selected = "What is Malaria?")
    updateTabsetPanel(session, "malaria_tabs", selected = "Geographic Distribution")
  })
  
  observeEvent(input$go_top, {
    updateNavbarPage(session, "main_nav", selected = "What is Malaria?")
    updateTabsetPanel(session, "malaria_tabs", selected = "Geographic Distribution")
    updateSliderInput(session, "top_year", value = input$year)
  })
  
  observeEvent(input$go_history, {
    updateNavbarPage(session, "main_nav", selected = "What is Malaria?")
    updateTabsetPanel(session, "malaria_tabs", selected = "History of Malaria")
  })
  
  observeEvent(input$go_case, {
    updateNavbarPage(session, "main_nav", selected = "Comparative Case Study (2000-2015)")
  })
  
  observeEvent(input$go_treatment, {
    updateNavbarPage(session, "main_nav", selected = "Treatment")
    updateTabsetPanel(session, "treatment_tabs", selected = "Resistance and Mutations")
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
    
    n_countries <- n_distinct(d$country)
    
    p <- ggplot(d, aes(x = year, y = incidence, color = country)) +
      geom_line(linewidth = 1.1)
    
    if (isTRUE(input$comp_points)) p <- p + geom_point(size = 2)
    
    p +
      scale_color_manual(values = rep_len(line_palette, n_countries)) +
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
  }, bg = panel_bg)
  
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
  }, digits = 1, striped = TRUE, hover = TRUE, width = "100%")
  
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
                  fill = "#f187d0", alpha = 0.25) +
      geom_line(aes(y = median, color = "Median country"), linewidth = 1.3)
    
    line_colors <- c("Median country" = "#FFADE5")
    
    if (has_hl) {
      h <- malaria_countries %>%
        filter(country == hl,
               between(year, input$trend_years[1], input$trend_years[2]))
      
      if (nrow(h) > 0) {
        p <- p + geom_line(data = h,
                           aes(y = incidence, color = hl),
                           linewidth = 1.3)
        line_colors <- c(line_colors, setNames("#FFD166", hl))
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
  }, bg = panel_bg)
  
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
      geom_col(fill = "#f187d0") +
      geom_text(aes(label = round(incidence, 0)), hjust = -0.15,
                size = 3.8, color = text_light) +
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
  }, bg = panel_bg)
  
  output$dl_top_png <- downloadHandler(
    filename = function() "highest_burden.png",
    content = function(file) {
      ggsave(file, plot = plot_top(), width = 10, height = 6,
             units = "in", dpi = 300)
    }
  )
  
  # ==========================================================
  # CASE STUDY: WHO EUROPEAN REGION, 2000-2015
  # (one page; the year slider "eu_year" drives the map,
  #  the Azerbaijan summary, and the scatter plot)
  # ==========================================================
  
  # The three case-study CSV files are found with find_data_file() (above)
  
  cs_error <- NULL
  
  cs <- tryCatch(local({
    
    read_cs <- function(f) {
      path <- find_data_file(f)
      if (is.null(path)) {
        stop("File not found: ", f, " (looked in ", getwd(), " and its subfolders)")
      }
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
    
    leaflet(options = leafletOptions(preferCanvas = TRUE, minZoom = 2)) %>%
      setView(lng = 35, lat = 48, zoom = 3) %>%
      addPolygons(
        data = cs$eu_world,
        group = "eu-base",
        fillColor = "#2a2547",
        fillOpacity = 1,
        color = "#3c3763",
        weight = 0.5,
        options = pathOptions(interactive = FALSE)
      ) %>%
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
      geom_line(color = "#f187d0", linewidth = 1.2, na.rm = TRUE) +
      geom_point(color = "#FFADE5", size = 2.5, na.rm = TRUE) +
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
  }, bg = panel_bg)
  
  output$cs_europe_trend <- renderPlot({
    cs_ready()
    cs_trend_plot(cs$europe_trend, "WHO European Region malaria estimates")
  }, bg = panel_bg)
  
  # ---- Azerbaijan ----
  
  output$az_summary <- renderUI({
    
    cs_ready()
    
    az <- cs$europe_cases %>%
      filter(iso3 == "AZE", year == as.integer(input$eu_year))
    
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
        div(class = "home-stat-value", input$eu_year),
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
  }, bg = panel_bg)
  
  # ---- Healthcare resources vs. malaria ----
  
  resource_year_data <- reactive({
    
    cs_ready()
    
    cs$resource_comparison %>%
      filter(year == as.integer(input$eu_year))
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
        color = "#FFD166",
        linewidth = 1.2,
        se = FALSE
      ) +
      geom_point(aes(color = is_azerbaijan), size = 3.5, alpha = 0.9) +
      geom_text(
        data = d %>% filter(is_azerbaijan),
        aes(label = "Azerbaijan"),
        color = "#FF3CAC",
        nudge_x = 0.1,
        hjust = 0,
        size = 4.5,
        show.legend = FALSE
      ) +
      scale_color_manual(
        values = c("FALSE" = "#B193CC", "TRUE" = "#FF3CAC"),
        labels = c("FALSE" = "Other countries", "TRUE" = "Azerbaijan"),
        name = NULL
      ) +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = paste("Healthcare resources vs. malaria burden:", input$eu_year),
        x = "Medical doctors per 10,000 people",
        y = "Estimated malaria cases per 100,000 people"
      ) +
      chart_theme
  }, bg = panel_bg)
  
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
      "For ", input$eu_year,
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
    
    leaflet(d, options = leafletOptions(minZoom = 1)) %>%
      addPolygons(
        data = world,
        fillColor = "#e8e6f0",
        fillOpacity = 1,
        color = "#b9b3cf",
        weight = 0.5,
        options = pathOptions(interactive = FALSE)
      ) %>%
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
  # K. TREATMENT PAGES: TABLES AND DOWNLOAD
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
    
    result %>%
      rename(any_of(c(
        "Drug" = "drug_name",
        "Records" = "records",
        "Countries" = "countries",
        "Median failure, per-protocol (%)" = "median_treatment_failure_pp",
        "Median failure, Kaplan-Meier (%)" = "median_treatment_failure_km",
        "Median day-3 positivity (%)" = "median_positive_day_3_days"
      )))
  })
  
  output$drug_summary <- renderTable({ drug_summary() },
                                     digits = 1, na = "",
                                     striped = TRUE, hover = TRUE, width = "100%")
  
  output$dl_summary <- downloadHandler(
    filename = function() "drug_summary.csv",
    content  = function(file) write_csv(drug_summary(), file)
  )
  
  # ---- Resistance and Mutations tables ----
  output$drug_marker_table <- renderTable(
    drug_marker_table, bordered = TRUE, striped = TRUE, width = "100%"
  )
  
  output$key_terms_table <- renderTable(
    key_terms_table, bordered = TRUE, striped = TRUE, width = "100%"
  )
  
  # ==========================================================
  # L. DATASET OVERVIEW
  # ==========================================================
  
  output$ds_records <- renderText(format(nrow(who), big.mark = ","))
  output$ds_countries <- renderText(n_distinct(na.omit(who$country_name)))
  output$ds_species <- renderText(n_distinct(na.omit(who$plasmodium_species)))
  output$ds_drugs <- renderText(n_distinct(na.omit(who$drug_name)))
  output$ds_years <- renderText(
    paste(year_range[1], year_range[2], sep = "\u2013")
  )
  
  output$ds_species_plot <- renderPlot({
    
    d <- species_counts %>% filter(!is.na(plasmodium_species))
    
    ggplot(d, aes(x = n, y = reorder(plasmodium_species, n))) +
      geom_col(fill = "#f187d0") +
      geom_text(aes(label = format(n, big.mark = ",")),
                hjust = -0.15, size = 3.8, color = text_light) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(title = "Records by parasite species", x = "Records", y = NULL) +
      chart_theme
  }, bg = panel_bg)
  
  output$ds_drug_plot <- renderPlot({
    
    d <- drug_counts %>%
      filter(!is.na(drug_name)) %>%
      slice_head(n = 10)
    
    ggplot(d, aes(x = n, y = reorder(drug_name, n))) +
      geom_col(fill = "#c2168c") +
      geom_text(aes(label = format(n, big.mark = ",")),
                hjust = -0.15, size = 3.8, color = text_light) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(title = "Ten most common drugs", x = "Records", y = NULL) +
      chart_theme
  }, bg = panel_bg)
  
  output$species_table <- renderTable({
    species_counts %>% rename(Species = plasmodium_species, Records = n)
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  output$drug_table <- renderTable({
    drug_counts %>% rename(Drug = drug_name, Records = n)
  }, striped = TRUE, hover = TRUE, width = "100%")
}

# ============================================================
# LAUNCH
# ============================================================

shinyApp(ui = ui, server = server)