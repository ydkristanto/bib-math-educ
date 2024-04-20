library(tidyverse)
library(bibliometrix)

paths <- list.files(path = "datasets/", full.names = TRUE)
paths <- setdiff(paths, c("datasets/bib_data_simple.RData"))
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

stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now")

rm(bib_data, paths)

save(bib_data_simple, stop_words, file = "datasets/bib_data_simple.RData")

