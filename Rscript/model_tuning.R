library(furrr)
future::plan("multiprocess")
set.seed(1212)

test_n_topics <- function(k, topic_prior, word_prior) {
    
    fit <- LDA$new(n_topics = k, doc_topic_prior = topic_prior, topic_word_prior = word_prior)
    
    doc_topic_distr <-
        fit$fit_transform(
            x = dtm,
            n_iter = 1000,
            convergence_tol = 0.001,
            n_check_convergence = 25,
            progressbar = FALSE
        )
    
    preds      <- apply(doc_topic_distr, 1, which.max)
    tidy_preds <- tibble(id = names(preds), topic = preds)
    
    split_by_topic <- tags %>% 
        left_join(tidy_preds, by = "id") %>% 
        group_split(topic)
    
    split_by_topic %>% 
        map_dbl(~ {
            count(.x, name, sort = TRUE) %>% 
                summarise(mu = mean(n^2)) %>% 
                pull(mu)
        }) %>% 
        sd()
}

params <-
    expand_grid(
        k = 11:20,
        topic_prior = seq(0.1, 1, .1),
        word_prior = seq(0.001, 0.01, 0.001)
    )

res <- furrr::future_pmap_dbl(params, test_n_topics)

params %>% 
    bind_cols(deviation = res) %>% 
    top_n(3, wt = -deviation)
