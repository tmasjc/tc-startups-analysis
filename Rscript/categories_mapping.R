library(tidyverse)
library(glue)
library(jsonlite)
library(progress)

# extract categories' id
ids <- readRDS("Data/sample.rds") %>% 
    map(pluck, "body", "categories") %>% 
    unlist() %>% 
    unique()

# fetch name by id via API
obtain_category <- function(id, delay = 1) {
    Sys.sleep(delay)
    url = glue("https://techcrunch.com/wp-json/wp/v2/categories/{id}")
    fromJSON(url)$name
}

# execute
pb   <- progress_bar$new(total = length(ids))
cats <- purrr::map_chr(ids, ~ { pb$tick(); obtain_category(.x) })

# clean up
cats <- map_chr(cats, ~ str_replace(.x, "&amp;", "&"))

# export
write_json(tibble(id = ids, name = cats), path = "Data/categories.json")
