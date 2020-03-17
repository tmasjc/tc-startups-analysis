library(tidyverse)
library(tidytext)
library(ggthemes)
library(igraph)
library(ggraph)
old <- theme_set(theme_minimal(base_family = "Menlo"))


# Load Data ---------------------------------------------------------------


# first 20 pages only
raw <- 
    readRDS("data/sample.rds") %>% 
    `[[`(1) %>% 
    `[[`("body") %>% 
    as_tibble()

# eda data frame
df <- 
    raw %>%
    # unlist and remove html tags
    mutate_at(vars("title", "content"), 
              ~ str_remove_all(.x[['rendered']], "<.*?>|\n|\\&#[0-9]{4}")) %>% 
    select(id, date, title, content, shortlink)


# Common Words ------------------------------------------------------------

manual_removals = c("yolo", "iframe")
stop_words <- tibble(word = manual_removals, lexicon = "manual") %>% 
    bind_rows(stop_words)

common_words <- 
    df %>% 
    select(id, content) %>% 
    unnest_tokens(word, content) %>% 
    anti_join(stop_words, by = "word")

common_words %>% 
    count(word, sort = TRUE) %>% 
    filter(n > 10, !word %in% manual_removals) %>%
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n)) + 
    geom_col(width = 0.3) + 
    coord_flip() + 
    labs(x = "", y = "Occurance")


# TF-IDF ------------------------------------------------------------------


common_terms <- df %>% 
    select(id, content) %>% 
    unnest_tokens(word, content) %>% 
    anti_join(stop_words, by = "word") %>% 
    count(id, word, sort = TRUE) %>% 
    filter(n > 1) %>% 
    bind_tf_idf(term = word, document = id, n = n) %>% 
    arrange(desc(tf_idf))

common_terms %>% 
    top_n(50, wt = tf_idf) %>% 
    ggplot(aes(reorder(word, tf_idf), tf_idf)) + 
    geom_col(width = 0.3) + 
    coord_flip() + 
    labs(x = "", y = "TF-IDF")


# N-gram ------------------------------------------------------------------


bigrams_filtr <- df %>% 
    select(id, content) %>% 
    # temp solution
    mutate(content = str_replace_all(content, "\\.", " ")) %>% 
    unnest_tokens(bigram, content, token = "ngrams", n = 2) %>% 
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    filter_at(vars("word1", "word2"), all_vars(!. %in% stop_words$word)) %>% 
    # remove amount relevant
    filter(!str_detect(word1, "^\\d+$"), word2 != "million")

bigrams_filtr %>% count(word1, word2, sort = TRUE)

# in bar chart form
bigrams_filtr %>% 
    unite(bigram, word1, word2, sep = " ") %>% 
    count(id, bigram) %>% 
    bind_tf_idf(bigram, id, n) %>% 
    top_n(50, wt = tf_idf) %>% 
    ggplot(aes(reorder(bigram, tf_idf), tf_idf)) + 
    geom_col(width = 0.3) + 
    coord_flip() + 
    labs(x = "", y = "TF-IDF")

# in graph form
bigrams_filtr %>% 
    count(word1, word2) %>% 
    filter(n > 1) %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = "fr") + 
    geom_edge_link(
        aes(edge_alpha = n), 
        show.legend = FALSE,
        arrow = grid::arrow(type = "open", length = unit(3, "mm")),
        end_cap = circle(1, "mm"),
        edge_colour = "navyblue"
        ) +
    geom_node_point(col = "darkblue", size = 3) + 
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, family = "Menlo") + 
    theme_void(base_family = "Menlo")


