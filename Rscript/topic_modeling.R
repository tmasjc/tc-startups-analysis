library(tidyverse)
library(text2vec)
library(stopwords)
library(LDAvis)
set.seed(1212)

# Tidy Data ---------------------------------------------------------------


# raw json 
raw <- readRDS("Data/sample.rds") 

# extract useful columns
extract_texts <- . %>%
    `[[`("body") %>% 
    as_tibble() %>% 
    # unlist and remove html tags
    mutate_at(vars("title", "content"), 
              ~ str_remove_all(.x[['rendered']], "<.*?>|\n|\\&#[0-9]{4}")) %>% 
    select(id, date, title, content, shortlink)

# each page contains 20 articles
df <- map_df(raw, extract_texts)

# extract categories tags for each post 
tags <- raw %>% 
    map(pluck, "body", "categories") %>% 
    purrr::flatten()
names(tags) <- df$id

# tag categories by name in tidy format
tags <- tags %>% 
    enframe(name = "id", value = "category") %>%
    unnest_longer("category") %>% 
    left_join(jsonlite::fromJSON("Data/categories.json"), 
              by = c("category" = "id")) %>% 
    # ignore publisher/funding related
    filter(
        !name %in% c(
            "Startups",
            "TC",
            "Extra Crunch",
            "Recent Funding",
            "Fundings & Exits",
            "Funding"
        )
    )

# Convert Text to Vector --------------------------------------------------


# convert all to lowercase and remove numbers (123, 1,500, 0.5, 0.777)
prep_func <- function(v) {
    str_replace_all(tolower(v), "\\d[[:punct:]\\d+]*", "")
}

# create iterators
itokens <- 
    itoken(
        iterable = df$content,
        preprocessor = prep_func,
        tokenizer = word_tokenizer,
        progressbar = TRUE,
        ids = df$id
    )

# make vocabulary
vocabs <- 
    itokens %>%
    create_vocabulary(stopwords = stopwords(source = "stopwords-iso")) %>%
    prune_vocabulary(term_count_min = 30, doc_proportion_max = 0.7)
vocabs

# document-term matrix
dtm <- 
    vocabs %>% 
    vocab_vectorizer() %>% 
    create_dtm(itokens, vectorizer = ., type = "dgTMatrix")

# fit LDA model here
fit <- LDA$new(n_topics = 16, doc_topic_prior = 0.8, topic_word_prior = 0.001)

# distribution of topics in documents
doc_topic_distr <-
    fit$fit_transform(
        x = dtm,
        n_iter = 1000,
        convergence_tol = 0.001,
        n_check_convergence = 25,
        progressbar = TRUE
    )


# Model Accessment ----------------------------------------------------------


# assign predicted topic to id
preds      <- apply(doc_topic_distr, 1, which.max)
tidy_preds <- tibble(id = names(preds), topic = preds)

split_by_topic <- tags %>% 
    left_join(tidy_preds, by = "id") %>% 
    group_split(topic)

# via LDAvis
fit$plot(reorder.topics = FALSE)

# via frequent tags per topic
split_by_topic %>% 
    map_df( ~ {
        count(.x, name, sort = TRUE) %>% 
            top_n(n = 3, wt = n)
    }, .id = "topic") %>% 
    mutate(topic = factor(as.numeric(topic), ordered = TRUE)) %>% 
    ggplot(aes(topic, n)) + 
    geom_label(aes(label = name)) +
    theme_minimal(base_family = "Menlo")
    



