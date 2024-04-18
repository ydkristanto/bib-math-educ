# Library ----
library(shiny)
library(bslib)
library(tidyverse)
library(plotly)

# Data ----
load(url("https://raw.githubusercontent.com/ydkristanto/bib-math-educ/main/datasets/bib_data_simple.RData"))

# Links ----
others_link <- tags$a(
  shiny::icon("shapes"),
  "Others",
  href = "https://people.usd.ac.id/~ydkristanto/index.php/media-pengajaran/shiny-stat-dan-id/",
  target = "_blank"
)
github_link <- tags$a(
  shiny::icon("github"),
  "Github",
  href = "https://github.com/ydkristanto/bib-math-educ",
  target = "_blank"
)

# User interface ----
ui <- page_navbar(
  title = "Bibliometric Analysis",
  id = "bib_analysis",
  ## Sidebar ----
  sidebar = sidebar(
    accordion(
      ### Analysis ----
      accordion_panel(
        title = "Analysis",
        selectInput(
          "analysis",
          div("Show:", style = "font-weight: bold;"),
          choices = c(
            "Authors",
            "Citations count",
            "Words",
            "Words over time",
            "Co-occurence network",
            "Collaboration network"
          ),
          selected = "Authors"
        ),
        conditionalPanel(
          "input.analysis == 'Words' | input.analysis == 'Words over time'",
          selectInput(
            "words_field",
            div("Field:", style = "font-weight: bold;"),
            c("Title", "Abstract", "Keywords"),
            selected = "Keywords"
          )
        ),
        conditionalPanel(
          "(input.analysis == 'Words' | input.analysis == 'Words over time') & (input.words_field == 'Title' | input.words_field == 'Abstract')",
          selectInput(
            "ngrams",
            div("N-grams:", style = "font-weight: bold;"),
            choices = c("Unigrams", "Bigrams", "Trigrams"),
            selected = "Unigrams"
          )
        )
      ),
      ### Filter ----
      accordion_panel(
        title = "Filter",
        checkboxGroupInput(
          "pub_name",
          div("Publication name:", style = "font-weight: bold;"),
          choices = c(
            "Educ. Stud. Math." = "EDUC. STUD. MATH.",
            "ZDM - Math. Educ." = "ZDM-MATH. EDUC.",
            "J. Res. Math. Educ." = "J. RES. MATH. EDUC.",
            "Int. J. Sci. Math. Educ." = "INT. J. SCI. MATH. EDUC.",
            "J. Math. Teach. Educ." = "J. MATH. TEACH. EDUC.",
            "Math. Think. Learn." = "MATH. THINK. LEARN.",
            "Ensen. Cienc." = "ENSEN. CIENC.",
            "Rev. Latinoam. Investig. Mat. Educ." = "REV. LATINOAM. INVESTIG. MAT. EDUC.",
            "Eurasia J. Math. Sci. Technol. Educ." = "EURASIA J. MATH. SCI. TECHNOL. EDUC.",
            "Bolema: Math. Educ. Bull." = "BOLEMA-MATH. EDUC. BULL."
          ),
          selected = c(
            "EDUC. STUD. MATH.",
            "ZDM-MATH. EDUC.",
            "J. RES. MATH. EDUC."
          )
        ),
        sliderInput(
          "year",
          div("Year", style = "font-weight: bold;"),
          min = 1986, max = 2024,
          value = c(2014, 2024), step = 1,
          ticks = FALSE
        ),
        textInput(
          "authors",
          div("Author names contains (e.g. Cai):", style = "font-weight: bold;")
        ),
        textInput(
          "title",
          div("Title contains (e.g. mathematics):", style = "font-weight: bold;")
        ),
        textInput(
          "abstract",
          div("Abstract contains (e.g. reasoning):", style = "font-weight: bold;")
        ),
        textInput(
          "keywords",
          div("Keywords contains (e.g. problem solving):", style = "font-weight: bold;")
        )
      ),
      multiple = FALSE,
      open = "Analysis"
    )
  ),
  ## Explorer ----
  nav_panel(
    title = "Explorer",
    card(
      card_header(
        "Plot"
      ),
      plotlyOutput("plot"),
      card_footer(
        "Plot of data"
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  ## Filter data ----
  bib_data <- reactive({
    source <- input$pub_name
    minyear <- input$year[1]
    maxyear <- input$year[2]
    
    # Apply filter
    dat <- bib_data_simple %>% 
      filter(
        source_abbr %in% pub_name,
        year >= minyear,
        year <= maxyear
      )
    
    # Filter by authors
    if (!is.null(input$authors) && input$authors != "") {
      authors_input <- input$authors %>% 
        as.character() %>% 
        toupper()
      dat <- dat %>% 
        filter(str_detect(authors, authors_input))
    }
    
    # Filter by title
    if (!is.null(input$title) && input$title != "") {
      title_input <- input$title %>% 
        as.character() %>% 
        toupper()
      dat <- dat %>% 
        filter(str_detect(title, title_input))
    }
    
    # Filter by abstract
    if (!is.null(input$abstract) && input$abstract != "") {
      abstract_input <- input$abstract %>% 
        as.character() %>% 
        toupper()
      dat <- dat %>% 
        filter(str_detect(abstract, abstract_input))
    }
    
    # Filter by keywords
    if (!is.null(input$keywords) && input$keywords != "") {
      keywords_input <- input$keywords %>% 
        as.character() %>% 
        toupper()
      dat <- dat %>% 
        filter(str_detect(author_keywords, keywords_input))
    }
    
    dat
  })
}

# App ----
shinyApp(ui, server)
