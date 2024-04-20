# Library ----
library(plotly)
library(GGally)
library(network)
library(sna)
library(tidyverse)
library(tnet)

# Data ----
load(url("https://raw.githubusercontent.com/ydkristanto/bib-math-educ/main/datasets/bib_data_simple.RData"))

# Data preparation ----
bib_data <- bib_data_simple$author_keywords %>%
  str_split("; ") %>%
  lapply(function(x) {
    expand.grid(x, x, w = 1 / length(x), stringsAsFactors = FALSE)
  }) %>%
  bind_rows() %>% 
  as_tibble()

bib_data <- apply(bib_data[, -3], 1, str_sort) %>%
  t() %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(w = bib_data$w) %>% 
  as_tibble()

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
top_keywords <- bib_data_simple %>% 
  select(author_keywords) %>% 
  separate_longer_delim(
    cols = author_keywords, delim = "; "
  ) %>% 
  group_by(author_keywords) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(author_keywords)) %>% 
  arrange(-n) %>% 
  head(100)

# Apply filter
bib_data <- bib_data %>% 
  filter(
    from %in% top_keywords$author_keywords,
    to %in% top_keywords$author_keywords
  )

# Create network ----
net <- network(bib_data, directed = FALSE,
               vertex.attrnames = top_keywords$author_keywords)
p <- ggnet2(net, size = 6)

ggplotly(p)

