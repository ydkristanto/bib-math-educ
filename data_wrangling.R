library(tidyverse)
library(bibliometrix)

paths <- list.files(path = "datasets/", full.names = TRUE)
bib_data <- bibliometrix::convert2df(
  file = paths,
  dbsource = "wos",
  format = "bibtex"
)
bib_data <- bib_data %>% 
  rownames_to_column(var = "cite_info") %>% 
  as_tibble()
bib_data_simple <- bib_data %>% 
  select(
    AU, DE, ID, AB, DI,
    JI, SO, LA, month, NR,
    PP, TC, TI, DT, VL,
    PY, AU_UN
  ) %>% 
  rename(
    authors = AU, author_keywords = DE,
    keyword_plus = ID, abstract = AB,
    doi = DI, source_abbr = JI,
    pub_name = SO, language = LA,
    n_references = NR, pages = PP,
    times_cited = TC, title = TI,
    type = DT, vol = VL,
    year = PY, author_affil = AU_UN
  )
save(bib_data_simple, file = "datasets/bib_data_simple.RData")

