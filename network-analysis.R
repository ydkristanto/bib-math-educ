# Library ----
library(plotly)
library(GGally)
library(network)
library(tidyverse)

# Data ----
load(url("https://raw.githubusercontent.com/ydkristanto/bib-math-educ/main/datasets/bib_data_simple.RData"))

# Data preparation ----
#' Create bib_data having three columns,
#' Var1, Var2, and w
bib_data <- bib_data_simple$author_keywords %>%
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
  ) %>% 
  mutate(
    from = tolower(from),
    to = tolower(to)
  )

# Creating network objects ----
## Node list ----
node_list <- top_keywords %>% 
  rowid_to_column("id") %>% 
  rename(
    label = author_keywords,
    occurrence = n
  ) %>% 
  mutate(
    label = tolower(label),
    label_w = ifelse(id <= 10, label, NA)
  )

## Edge list ----
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

# Creating network objects ----
bib_network <- network(
  edge_list,
  vertex.attr = node_list,
  directed = FALSE,
  matrix.type = "edgelist",
  ignore.eval = FALSE
)

# Network visualization ----
plot0 <- ggnet2(
  bib_network,
  node.size = "occurrence",
  node.color = "label",
  label = "label_w",
  edge.size = "weight",
  edge.color = "gray",
  edge.alpha = .1
)

plot <- ggplotly(
  plot0,
  tooltip = c("color", "size"),
  legend = "none"
) %>% 
  style(showlegend = FALSE)


