library(tidyverse)
library(widyr)
library(igraph)
library(ggraph)
library(visNetwork)
set.seed(1111)

# raw json 
raw <- readRDS("Data/sample.rds") 

# extract categories for each post 
cats <- raw %>% 
    map(pluck, "body", "categories") %>% 
    purrr::flatten()

ids <- raw %>% 
    map(pluck, "body", "id") %>% 
    purrr::flatten()

names(cats) <- ids

# tag categories by name in tidy format
df <- cats %>% 
    enframe(name = "id", value = "category") %>%
    unnest_longer("category") %>% 
    left_join(jsonlite::fromJSON("Data/categories.json"), 
              by = c("category" = "id")) %>% 
    # remove TC (Techcrunch)
    filter(name != "TC")

# find pairwise correlation
cors <- df %>% pairwise_cor(name, id, sort = TRUE, upper = FALSE)

# network graph
cors %>% 
    filter(!is.nan(correlation), correlation > 0) %>%
    graph_from_data_frame() %>% 
    ggraph(layout = "fr") + 
    geom_edge_link(aes(edge_alpha = correlation),
                   edge_colour = "navyblue",
                   show.legend = FALSE) +
    geom_node_point(color = "darkblue", size = 3) + 
    geom_node_text(
        aes(label = name),
        repel = TRUE,
        family = "Menlo",
        size = 3,
        point.padding = unit(0.2, "lines")
    ) + 
    theme_void()

# example: Advertising Tech
cors %>% 
    filter(item1 == "Advertising Tech") %>% 
    filter(!is.nan(correlation)) %>% 
    ggplot(aes(reorder(item2, correlation), correlation)) + 
    geom_col(width = 0.3, col = "navyblue") + 
    coord_flip() + 
    theme_minimal(base_family = "Menlo") +
    labs(
        x = "",
        y = "",
        title = "Binary Correlation of Categories for 'Advertising Tech'",
        subtitle = "source: https://techcrunch.com/startups/"
    )

# interactive via visNetwork
nodes <- c(cors$item1, cors$item2) %>% 
    unique() %>% 
    sort() %>% 
    data.frame(id = .) %>% 
    mutate(title = paste("Cat:", id))

edges <- cors %>% 
    rename(from = item1, to = item2) %>% 
    filter(abs(correlation) > 0.1) %>% 
    mutate(title = paste("Phi:", prettyNum(correlation, digits = 2)),
           width = correlation * 5,
           color = ifelse(correlation > 0, "#708090", "#FF0000"))

visNetwork(nodes, edges, width = "1600px", height = "800px") %>% 
    visIgraphLayout(layout = "layout_in_circle") %>% 
    visNodes(size = 20, color = "#696969") %>% 
    visOptions(highlightNearest = list(enabled = T, hover = T), 
               nodesIdSelection = T)

