library(tidyverse)
library(tidytext)
library(ggthemes)
old <- theme_set(theme_minimal(base_family = "Menlo"))

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

txts <- 
    df %>% 
    select(id, content) %>% 
    unnest_tokens(word, content) %>% 
    anti_join(stop_words, by = "word")

manual_removals = c("yolo", "iframe")
txts %>% 
    count(word, sort = TRUE) %>% 
    filter(n > 10, !word %in% manual_removals) %>%
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n)) + 
    geom_col(width = 0.3) + 
    coord_flip() + 
    labs(x = "", y = "Occurance")


