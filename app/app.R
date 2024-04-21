# Library ----
library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(sna)
library(GGally)
library(network)

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
            "Co-occurrence network",
            "Co-authors network"
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
            "input.analysis == 'Authors' | input.analysis == 'Citations count'",
            numericInput(
              "author_count",
              div("Number of authors:", style = "font-weight: bold;"),
              value = 10,
              min = 2,
              max = 20
            )
          ),
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
          conditionalPanel(
            "input.analysis == 'Co-occurrence network' | input.analysis == 'Co-authors network'",
            numericInput(
              "node_count",
              div("Number of nodes:", style = "font-weight: bold;"),
              value = 100,
              min = 1,
              max = 200
            ),
            numericInput(
              "node_labelled",
              div("Number of labelled nodes:", style = "font-weight: bold;"),
              value = 10,
              min = 1,
              max = 20
            )
          ),
          placement = "left"
        ),
        class = "d-flex justify-content-between"
      ),
      plotlyOutput("plot"),
      card_footer(
        div(
          "Source: ",
          tags$span(
            a(
              "Web of Science Database",
              href = "https://www.webofscience.com/wos",
              target = "_blank"
            ),
            style = "font-weight: bold;"
          ),
          " (retrieved on 15 April 2024)",
          style = "font-size: 0.75em;"
        )
      ),
      full_screen = TRUE
    ),
    icon = shiny::icon("chart-simple")
  ),
  ## Information ----
  nav_panel(
    "Information",
    layout_column_wrap(
      width = 1 / 2,
      navset_card_underline(
        ### About ----
        nav_panel(
          title = "About",
          p("This web-based application allows you to conduct bibliometric analysis in the field of mathematics education. Through this application, for example, you can identify prolific and relevant authors. Additionally, you can also identify research trends in the field of mathematics education.")
        ),
        nav_panel(
          ### Tools ----
          title = "Tools",
          p("This dashboard was developed using the ", a("R programming language", href = "https://www.r-project.org/", target = "_blank"), " and the ", a("{Shiny}", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), " package. The ", a("{shinylive}", href = "https://CRAN.R-project.org/package=shinylive", target = "_blank"), " package was utilized to export this application so it can be run in a web browser without a separate R server. The dashboard layout is structured using ", a("{bslib}.", href = "https://CRAN.R-project.org/package=bslib", target = "_blank")),
          p("The ", a("{network}", href = "https://CRAN.R-project.org/package=network", target = "_blank"), " and ", a("{sna}", href = "https://CRAN.R-project.org/package=sna", target = "_blank"), " packages were used to conduct network analysis [see, ", tooltip(a("Kristanto & Padmi (2020)", href = "https://doi.org/10.21831/pep.v24i2.33912", target = "_blank"), "Using network analysis for rapid, transparent, and rigorous thematic analysis: A case study of online distance learning"), " for a brief introduction on using this method in analyzing textual data]. All statistical charts in this dashboard are created using the ", a("{ggplot2},", href = "https://ggplot2.tidyverse.org", target = "_blank"), " ", a("{GGally},", href = "https://CRAN.R-project.org/package=GGally", target = "_blank"), " and ", a("{plotly}", href = "https://plotly-r.com", target = "_blank"), " packages.")
        ),
        nav_panel(
          ### Developer ----
          title = "Developer",
          p("The developer and maintainer of this application is ", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto", target = "_blank"), " a lecturer and researcher in ", a("the Mathematics Education department", href = "https://usd.ac.id/s1pmat", target = "_blank"), " at ", a("Sanata Dharma University,", href = "https://www.usd.ac.id/", target = "_blank"), " Yogyakarta, Indonesia.")
        ),
        nav_panel(
          ### Source Code ----
          title = "Source Code",
          p("The source code of this application is available on ", a("GitHub repository.", href = "https://github.com/ydkristanto/bib-math-educ", target = "_blank"), " If you would like to report any issues or request additional features for this application, please ", a("create an issue", href = "https://github.com/ydkristanto/bib-math-educ/issues", target = "_blank"), " or, even better, submit ", a("a pull request", href = "https://github.com/ydkristanto/bib-math-educ/pulls", target = "_blank"), " in the repository.")
        )
      ),
      ### Data ----
      card(
        card_header("Data"),
        card_body(
          p("The bibliographic data used in this application was obtained from the ", a("Web of Science database", href = "https://www.webofscience.com/wos", target = "_blank"), " (thanks to ", a("Johannes Kepler University,", href = "https://www.jku.at/en", target = "_blank"), " Linz, for providing the access.) The data was downloaded on 15 April 2024. It contains bibliographic information from scientific articles published in the following journals."),
          tags$ul(
            tags$li(a("Educational Studies in Mathematics", href = "https://link.springer.com/journal/10649", target = "_blank")),
            tags$li(a("ZDM - Mathematics Education", href = "https://link.springer.com/journal/11858", target = "_blank")),
            tags$li(a("Journal for Research in Mathematics Education", href = "https://pubs.nctm.org/view/journals/jrme/jrme-overview.xml", target = "_blank")),
            tags$li(a("International Journal of Science and Mathematics Education", href = "https://link.springer.com/journal/10763", target = "_blank")),
            tags$li(a("Journal of Mathematics Teacher Education", href = "https://link.springer.com/journal/10857", target = "_blank")),
            tags$li(a("Mathematical Thinking and Learning", href = "https://www.tandfonline.com/journals/hmtl20", target = "_blank")),
            tags$li(a("Enseñanza de las Ciencias", href = "https://ensciencias.uab.es/", target = "_blank")),
            tags$li(a("Revista Latinoamericana de Investigación en Matemática Educativa - RELIME", href = "https://www.relime.org/index.php/relime", target = "_blank")),
            tags$li(a("Eurasia Journal of Mathematics Science and Technology Education", href = "https://www.ejmste.com/", target = "_blank")),
            tags$li(a("Bolema - Boletim de Educação Matemática", href = "https://www.periodicos.rc.biblioteca.unesp.br/index.php/bolema", target = "_blank"))
          )
        )
      )
    ),
    icon = shiny::icon("circle-info")
  ),
  nav_spacer(),
  ## Nav menu ----
  nav_menu(
    title = "Links",
    nav_item(others_link),
    nav_item(github_link),
    icon = shiny::icon("link"),
    align = "right"
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
    author_count <- input$author_count
    node_count <- input$node_count
    node_labelled <- input$node_labelled
    
    ### Most relevant authors ----
    if(analysis_input == "Authors") {
      data <- bib_data()$authors %>%
        str_split(";") %>%
        unlist() %>%
        table() %>%
        as_tibble() %>%
        arrange(-n) %>%
        filter(str_detect(., "ANONYMOUS", negate = TRUE)) %>% 
        head(author_count)
      names(data)[1] <- "author"
      plot0 <- data %>% 
        mutate(author = fct_reorder(author, n)) %>%
        ggplot(aes(x = author, y = n)) +
        geom_col(aes(fill = n), show.legend = FALSE) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            face = "bold",
            hjust = .5
          ),
          axis.title.y = element_blank()
        ) +
        labs(
          title = "Most Relevant Authors",
          y = "Document count"
        ) +
        coord_flip()
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Citations count") {
      ### Most global cited authors ----
      data <- bib_data() %>% 
        separate_longer_delim(authors, delim = ";") %>% 
        group_by(authors) %>% 
        summarise(citation = sum(times_cited)) %>% 
        arrange(-citation) %>% 
        head(author_count)
      plot0 <- data %>% 
        mutate(authors = fct_reorder(authors, citation)) %>%
        ggplot(aes(x = authors, y = citation)) +
        geom_col(aes(fill = citation), show.legend = FALSE) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            face = "bold",
            hjust = .5
          ),
          axis.title.y = element_blank()
        ) +
        labs(
          title = "Most Global Cited Authors",
          y = "Citation count"
        ) +
        coord_flip()
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Words" && 
              input$chart_words == "Bar chart" &&
              input$field_words == "Keywords") {
      ### Most frequent words ----
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
        theme(
          plot.title = element_text(
            face = "bold",
            hjust = .5
          ),
          axis.title.y = element_blank()
        ) +
        labs(
          title = "Most Frequent Words",
          y = "Count"
        ) +
        coord_flip()
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Words" && 
              input$chart_words == "Trendline" &&
              input$field_words == "Keywords") {
      ### Words frequency over time ----
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
        theme(
          legend.position = "bottom",
          plot.title = element_text(
            face = "bold",
            hjust = .5
          )
        ) +
        labs(
          title = "Words Frequency Over Time",
          x = "Year",
          y = "Count"
        )
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Words" && 
              input$chart_words == "Bar chart" &&
              input$field_words == "Title") {
      ### Most frequent words ----
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
        theme(
          plot.title = element_text(
            face = "bold",
            hjust = .5
          ),
          axis.title.y = element_blank()
        ) +
        labs(
          title = "Most Frequent Words",
          y = "Count"
        ) +
        coord_flip()
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Words" && 
              input$chart_words == "Trendline" &&
              input$field_words == "Title") {
      ### Words frequency over time ----
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
        theme(
          plot.title = element_text(
            face = "bold",
            hjust = .5
          ),
          legend.position = "bottom"
        ) +
        labs(
          title = "Words Frequency Over Time",
          x = "Year",
          y = "Count"
        )
      plot <- ggplotly(plot0)
      
    } else if(analysis_input == "Words" && 
              input$chart_words == "Bar chart" &&
              input$field_words == "Abstract") {
      ### Most frequent words ----
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
        theme(
          plot.title = element_text(
            face = "bold",
            hjust = .5
          ),
          axis.title.y = element_blank()
        ) +
        labs(
          title = "Most Frequent Words",
          y = "Count"
        ) +
        coord_flip()
      plot <- ggplotly(plot0)
    } else if(analysis_input == "Words" && 
              input$chart_words == "Trendline" &&
              input$field_words == "Abstract") {
      ### Words frequency over time ----
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
        theme(
          plot.title = element_text(
            face = "bold",
            hjust = .5
          ),
          legend.position = "bottom"
        ) +
        labs(
          title = "Words Frequency Over Time",
          x = "Year",
          y = "Count"
        )
      plot <- ggplotly(plot0)
    } else if(input$analysis == "Co-occurrence network") {
      ### Co-occurrence network ----
      # Data preparation
      bib_data <- bib_data()$author_keywords %>%
        str_split("; ") %>%
        lapply(function(x) {
          expand.grid(x, x, w = 1 / length(x), stringsAsFactors = FALSE)
        }) %>%
        bind_rows() %>% 
        as_tibble()
      
      # Sorting bib_data
      bib_data <- apply(bib_data[, -3], 1, str_sort) %>%
        t() %>%
        data.frame(stringsAsFactors = FALSE) %>%
        mutate(w = bib_data$w) %>% 
        as_tibble()
      
      # Simplifying bib_data
      bib_data <- bib_data %>% 
        group_by(X1, X2) %>%
        summarise(w = sum(w), .groups = "drop") %>%
        filter(X1 != X2) %>% 
        distinct() %>% 
        rename(
          from = X1,
          to = X2,
          weight = w
        )
      
      # Filter
      top_keywords <- bib_data() %>% 
        select(author_keywords) %>% 
        separate_longer_delim(
          cols = author_keywords, delim = "; "
        ) %>% 
        group_by(author_keywords) %>% 
        summarise(n = n()) %>% 
        filter(!is.na(author_keywords)) %>% 
        arrange(-n) %>% 
        head(node_count)
      
      # Apply filter
      bib_data <- bib_data %>% 
        filter(
          from %in% top_keywords$author_keywords,
          to %in% top_keywords$author_keywords
        ) %>% 
        mutate(
          from = tolower(from),
          to = tolower(to)
        )
      
      # Creating network objects
      node_list <- top_keywords %>% 
        rowid_to_column("id") %>% 
        rename(
          label = author_keywords,
          occurrence = n
        ) %>% 
        mutate(
          label = tolower(label),
          label_w = ifelse(id <= node_labelled, label, NA)
        )
      
      edge_list <- left_join(
        bib_data, node_list, by = c("from" = "label")
      ) %>% 
        select(-from, -occurrence, -label_w) %>% 
        rename(from = id)
      
      edge_list <- edge_list %>% 
        left_join(node_list, by = c("to" = "label")) %>% 
        select(-to, -occurrence, -label_w) %>% 
        rename(to = id)
      
      edge_list <- edge_list %>% 
        select(from, to, weight)
      
      # Creating network objects
      bib_network <- network(
        edge_list,
        vertex.attr = node_list,
        directed = FALSE,
        matrix.type = "edgelist",
        ignore.eval = FALSE
      )
      
      # Network visualization
      plot0 <- ggnet2(
        bib_network,
        node.size = "occurrence",
        node.color = "label",
        label = "label_w",
        edge.size = "weight",
        edge.color = "gray",
        edge.alpha = .1
      ) +
        theme(
          plot.title = element_text(
            face = "bold",
            hjust = .5
          )
        ) +
        labs(title = "Co-occurrence Network")
      
      plot <- ggplotly(
        plot0,
        tooltip = c("color", "size"),
        legend = "none"
      ) %>% 
        style(showlegend = FALSE)
    } else if(input$analysis == "Co-authors network") {
      ### Collaboration network ----
      # Data preparation
      bib_data <- bib_data()$authors %>%
        str_split(";") %>%
        lapply(function(x) {
          expand.grid(x, x, w = 1 / length(x), stringsAsFactors = FALSE)
        }) %>%
        bind_rows() %>% 
        as_tibble()
      
      # Sorting bib_data
      bib_data <- apply(bib_data[, -3], 1, str_sort) %>%
        t() %>%
        data.frame(stringsAsFactors = FALSE) %>%
        mutate(w = bib_data$w) %>% 
        as_tibble()
      
      # Simplifying bib_data
      bib_data <- bib_data %>% 
        group_by(X1, X2) %>%
        summarise(w = sum(w), .groups = "drop") %>%
        filter(X1 != X2) %>% 
        distinct() %>% 
        rename(
          from = X1,
          to = X2,
          weight = w
        )
      
      # Filter
      top_authors <- bib_data() %>% 
        select(authors) %>% 
        separate_longer_delim(
          cols = authors, delim = ";"
        ) %>% 
        group_by(authors) %>% 
        summarise(n = n()) %>% 
        filter(!is.na(authors)) %>% 
        arrange(-n) %>% 
        head(node_count)
      
      # Apply filter
      bib_data <- bib_data %>% 
        filter(
          from %in% top_authors$authors,
          to %in% top_authors$authors
        ) %>% 
        mutate(
          from = tolower(from),
          to = tolower(to)
        )
      
      # Creating network objects
      node_list <- top_authors %>% 
        rowid_to_column("id") %>% 
        rename(
          label = authors,
          occurrence = n
        ) %>% 
        mutate(
          label = tolower(label),
          label_w = ifelse(id <= node_labelled, label, NA)
        )
      
      edge_list <- left_join(
        bib_data, node_list, by = c("from" = "label")
      ) %>% 
        select(-from, -occurrence, -label_w) %>% 
        rename(from = id)
      
      edge_list <- edge_list %>% 
        left_join(node_list, by = c("to" = "label")) %>% 
        select(-to, -occurrence, -label_w) %>% 
        rename(to = id)
      
      edge_list <- edge_list %>% 
        select(from, to, weight)
      
      # Creating network objects
      bib_network <- network(
        edge_list,
        vertex.attr = node_list,
        directed = FALSE,
        matrix.type = "edgelist",
        ignore.eval = FALSE
      )
      
      # Network visualization
      plot0 <- ggnet2(
        bib_network,
        node.size = "occurrence",
        node.color = "label",
        label = "label_w",
        edge.size = "weight",
        edge.color = "gray",
        edge.alpha = .1
      ) +
        theme(
          plot.title = element_text(
            face = "bold",
            hjust = .5
          )
        ) +
        labs(title = "Collaboration Network")
      
      plot <- ggplotly(
        plot0,
        tooltip = c("color", "size"),
        legend = "none"
      ) %>% 
        style(showlegend = FALSE)
    }
    
    plot
  })
}

# App ----
shinyApp(ui, server)
