# Library ----
library(shiny)
library(bslib)
library(tidyverse)
library(plotly)

# Data ----
load(url("https://raw.githubusercontent.com/ydkristanto/bib-math-educ/main/datasets/bib_data_simple.RData"))
stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now")

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
            "Co-occurence network",
            "Collaboration network"
          ),
          selected = "Authors"
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
          ticks = FALSE,
          sep = ""
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
        "Plot",
        popover(
          trigger = icon("gear"),
          title = "Pengaturan Plot",
          conditionalPanel(
            "input.analysis == 'Words'",
            selectInput(
              "field_words",
              div("Field:", style = "font-weight: bold;"),
              c("Title", "Abstract", "Keywords"),
              selected = "Keywords"
            ),
            selectInput(
              "chart_words",
              div("Chart:", style = "font-weight: bold;"),
              c("Bar chart", "Trendline"),
              selected = "Bar chart"
            )
          ),
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotlyOutput("plot"),
      card_footer(
        "Source: Web of Science Database"
      ),
      full_screen = TRUE
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
        source_abbr %in% source,
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
  
  ## plot ----
  output$plot <- renderPlotly({
    analysis_input <- input$analysis
    
    if(analysis_input == "Authors") {
      data <- bib_data()$authors %>%
        str_split(";") %>%
        unlist() %>%
        table() %>%
        as_tibble() %>%
        arrange(-n) %>%
        filter(str_detect(., "ANONYMOUS", negate = TRUE)) %>% 
        head(10)
      names(data)[1] <- "author"
      plot0 <- data %>% 
        mutate(author = fct_reorder(author, n)) %>%
        ggplot(aes(x = author, y = n)) +
        geom_col(aes(fill = n), show.legend = FALSE) +
        theme_minimal() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Document count") +
        coord_flip()
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Citations count") {
      data <- bib_data() %>% 
        separate_longer_delim(authors, delim = ";") %>% 
        group_by(authors) %>% 
        summarise(citation = sum(times_cited)) %>% 
        arrange(-citation) %>% 
        head(10)
      plot0 <- data %>% 
        mutate(authors = fct_reorder(authors, citation)) %>%
        ggplot(aes(x = authors, y = citation)) +
        geom_col(aes(fill = citation), show.legend = FALSE) +
        theme_minimal() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Citation count") +
        coord_flip()
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Words" && 
              input$chart_words == "Bar chart" &&
              input$field_words == "Keywords") {
      data <- bib_data() %>%
        select(author_keywords) %>% 
        separate_longer_delim(author_keywords, delim = "; ") %>% 
        group_by(author_keywords) %>% 
        summarise(n = n()) %>% 
        filter(!is.na(author_keywords)) %>% 
        arrange(-n) %>% 
        head(10)
      plot0 <- data %>% 
        mutate(author_keywords = fct_reorder(author_keywords, n)) %>% 
        ggplot(aes(x = author_keywords, y = n)) +
        geom_col(aes(fill = n), show.legend = FALSE) +
        theme_minimal() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Count") +
        coord_flip()
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Words" && 
              input$chart_words == "Trendline" &&
              input$field_words == "Keywords") {
      maxyear <- input$year[2]
      minyear <- maxyear - 10
      
      data_filter <- bib_data() %>%
        select(author_keywords) %>% 
        separate_longer_delim(author_keywords, delim = "; ") %>% 
        group_by(author_keywords) %>% 
        summarise(n = n()) %>% 
        filter(!is.na(author_keywords)) %>% 
        arrange(-n) %>% 
        head(5)
      data <- bib_data() %>% 
        select(year, author_keywords) %>% 
        separate_longer_delim(author_keywords, delim = "; ") %>% 
        group_by(year, author_keywords) %>% 
        summarise(n = n()) %>% 
        filter(!is.na(author_keywords)) %>% 
        arrange(-year, -n) %>% 
        filter(
          author_keywords %in% data_filter$author_keywords,
          year >= minyear,
          year <= maxyear
        )
      plot0 <- data %>% 
        ggplot(aes(x = year, y = n)) +
        geom_line(aes(group = author_keywords, color = author_keywords)) +
        scale_color_discrete(name = "Author Keyword") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        labs(x = "Year", y = "Count")
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Words" && 
              input$chart_words == "Bar chart" &&
              input$field_words == "Title") {
      data <- bib_data() %>%
        select(title) %>% 
        separate_longer_delim(title, delim = " ") %>% 
        group_by(title) %>% 
        summarise(n = n()) %>% 
        filter(!(title %in% toupper(stop_words))) %>% 
        arrange(-n) %>% 
        head(10)
      plot0 <- data %>% 
        mutate(title = fct_reorder(title, n)) %>% 
        ggplot(aes(x = title, y = n)) +
        geom_col(aes(fill = n), show.legend = FALSE) +
        theme_minimal() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Count") +
        coord_flip()
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Words" && 
              input$chart_words == "Trendline" &&
              input$field_words == "Title") {
      maxyear <- input$year[2]
      minyear <- maxyear - 10
      data_filter <- bib_data() %>%
        select(title) %>% 
        separate_longer_delim(title, delim = " ") %>% 
        filter(!(title %in% toupper(stop_words))) %>% 
        group_by(title) %>% 
        summarise(n = n()) %>% 
        filter(!is.na(title)) %>% 
        arrange(-n) %>% 
        head(5)
      
      data <- bib_data() %>% 
        select(title, year) %>% 
        separate_longer_delim(title, delim = " ") %>% 
        group_by(year, title) %>% 
        summarise(n = n()) %>% 
        filter(!(title %in% toupper(stop_words))) %>% 
        filter(
          title %in% data_filter$title,
          year >= minyear,
          year <= maxyear
        )
      
      plot0 <- data %>% 
        ggplot(aes(x = year, y = n)) +
        geom_line(aes(group = title, color = title)) +
        scale_color_discrete(name = "Word") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        labs(x = "Year", y = "Count")
      plot <- ggplotly(plot0)
      
    } else if(analysis_input == "Words" && 
              input$chart_words == "Bar chart" &&
              input$field_words == "Abstract") {
      data <- bib_data() %>%
        select(abstract) %>% 
        separate_longer_delim(abstract, delim = " ") %>% 
        group_by(abstract) %>% 
        summarise(n = n()) %>% 
        filter(!(abstract %in% toupper(stop_words))) %>% 
        arrange(-n) %>% 
        head(10)
      plot0 <- data %>% 
        mutate(abstract = fct_reorder(abstract, n)) %>% 
        ggplot(aes(x = abstract, y = n)) +
        geom_col(aes(fill = n), show.legend = FALSE) +
        theme_minimal() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Count") +
        coord_flip()
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Words" && 
              input$chart_words == "Trendline" &&
              input$field_words == "Abstract") {
      maxyear <- input$year[2]
      minyear <- maxyear - 10
      data_filter <- bib_data() %>%
        select(abstract) %>% 
        separate_longer_delim(abstract, delim = " ") %>% 
        filter(!(abstract %in% toupper(stop_words))) %>% 
        group_by(abstract) %>% 
        summarise(n = n()) %>% 
        filter(!is.na(abstract)) %>% 
        arrange(-n) %>% 
        head(5)
      
      data <- bib_data() %>% 
        select(abstract, year) %>% 
        separate_longer_delim(abstract, delim = " ") %>% 
        group_by(year, abstract) %>% 
        summarise(n = n()) %>% 
        filter(!(abstract %in% toupper(stop_words))) %>% 
        filter(
          abstract %in% data_filter$abstract,
          year >= minyear,
          year <= maxyear
        )
      
      plot0 <- data %>% 
        ggplot(aes(x = year, y = n)) +
        geom_line(aes(group = abstract, color = abstract)) +
        scale_color_discrete(name = "Word") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        labs(x = "Year", y = "Count")
      plot <- ggplotly(plot0)
    } else if(input$analysis == "Co-occurence network") {
      data <- bib_data()$author_keywords %>%
        str_split("; ") %>%
        lapply(function(x) {
          expand.grid(x, x, w = 1 / length(x), stringsAsFactors = FALSE)
        }) %>%
        bind_rows() %>% 
        as_tibble()
      data <- apply(data[, -3], 1, str_sort()) %>%
        t() %>%
        data.frame(stringsAsFactors = FALSE) %>%
        mutate(w = data$w)
      data <- data %>% 
        group_by(Var1, Var2) %>%
        summarise(w = sum(w)) %>%
        filter(Var1 != Var2)
    }
    
    plot
  })
}

# App ----
shinyApp(ui, server)
