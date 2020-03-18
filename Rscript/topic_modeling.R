library(tidyverse)
library(text2vec)
library(stopwords)
library(LDAvis)

# raw json 
raw <- readRDS("Data/sample.rds") 

# extract useful columns
format_cols <- . %>%
    `[[`("body") %>% 
    as_tibble() %>% 
    # unlist and remove html tags
    mutate_at(vars("title", "content"), 
              ~ str_remove_all(.x[['rendered']], "<.*?>|\n|\\&#[0-9]{4}")) %>% 
    select(id, date, title, content, shortlink)

# each page contains 20 articles
df <- map_df(raw, format_cols)

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
    prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.7)
vocabs

# document-term matrix
dtm <- 
    vocabs %>% 
    vocab_vectorizer() %>% 
    create_dtm(itokens, vectorizer = ., type = "dgTMatrix")

# fit LDA model here
fit <- LDA$new(n_topics = 25, doc_topic_prior = 0.1, topic_word_prior = 0.01)

# distribution of topics in documents
doc_topic_distr <-
    fit$fit_transform(
        x = dtm,
        n_iter = 1000,
        convergence_tol = 0.001,
        n_check_convergence = 25,
        progressbar = TRUE
    )

# plot via LDAvis
fit$plot()

