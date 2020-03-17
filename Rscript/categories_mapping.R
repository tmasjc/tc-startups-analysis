library(tidyverse)
library(glue)
library(jsonlite)

# extract categories' id
ids <- 
    readRDS("data/sample.rds") %>% 
    `[[`(1) %>% 
    `[[`(c("body", "categories")) %>% 
    unlist() %>% 
    unique()

# fetch name by id via API
obtain_category <- function(id) {
    url = glue("https://techcrunch.com/wp-json/wp/v2/categories/{id}")
    fromJSON(url)$name
}
cats <- tibble(id = ids, name = map_chr(ids, obtain_category))

# export
write_json(cats, path = "Data/categories.json")
