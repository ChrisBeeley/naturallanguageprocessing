
# based on https://juliasilge.com/blog/word-vectors-take-two/

library(tidyverse)
library(tidytext)
library(widyr)
library(stringr)

load("cleanData.Rdata")

hacker_news_text <- use_data$Improve %>%
  as_tibble() %>%
  mutate(text = str_replace_all(value, "&quot;|&#x2F;", "'"),    ## hex encoding
         text = str_replace_all(value, "&#x2F;", "/"),           ## more hex
         text = str_replace_all(value, "<a(.*?)>", " "),         ## links 
         text = str_replace_all(value, "&gt;|&lt;", " "),        ## html yuck
         text = str_replace_all(value, "<[^>]*>", " "),          ## mmmmm, more html yuck
         postID = row_number()) %>% 
  select(-value)

slide_windows <- function(tbl, doc_var, window_size) {
  # each word gets a skipgram (window_size words) starting on the first
  # e.g. skipgram 1 starts on word 1, skipgram 2 starts on word 2
  
  each_total <- tbl %>% 
    group_by(!!doc_var) %>% 
    mutate(doc_total = n(),
           each_total = pmin(doc_total, window_size, na.rm = TRUE)) %>%
    pull(each_total)
  
  rle_each <- rle(each_total)
  counts <- rle_each[["lengths"]]
  counts[rle_each$values != window_size] <- 1
  
  # each word get a skipgram window, starting on the first
  # account for documents shorter than window
  id_counts <- rep(rle_each$values, counts)
  window_id <- rep(seq_along(id_counts), id_counts)
  
  
  # within each skipgram, there are window_size many offsets
  indexer <- (seq_along(rle_each[["values"]]) - 1) %>%
    map2(rle_each[["values"]] - 1,
         ~ seq.int(.x, .x + .y)) %>% 
    map2(counts, ~ rep(.x, .y)) %>%
    flatten_int() +
    window_id
  
  tbl[indexer, ] %>%
    bind_cols(data_frame(window_id)) %>%
    group_by(window_id) %>%
    filter(n_distinct(!!doc_var) == 1) %>%
    ungroup
}

tidy_pmi <- hacker_news_text %>%
  unnest_tokens(word, text) %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  select(-n) %>%
  slide_windows(quo(postID), 8) %>%
  pairwise_pmi(word, window_id)

tidy_pmi

tidy_word_vectors <- tidy_pmi %>%
  widely_svd(item1, item2, pmi, nv = 256, maxit = 1000)

nearest_synonyms <- function(df, token) {
  df %>%
    widely(~ . %*% (.[token, ]), sort = TRUE)(item1, dimension, value) %>%
    select(-item2)
}

tidy_word_vectors %>%
  nearest_synonyms("nurse")

tidy_word_vectors %>%
  filter(dimension <= 24) %>%
  group_by(dimension) %>%
  top_n(12, abs(value)) %>%
  ungroup %>%
  mutate(item1 = reorder(item1, value)) %>%
  group_by(dimension, item1) %>%
  arrange(desc(value)) %>%
  ungroup %>%
  mutate(item1 = factor(paste(item1, dimension, sep = "__"), 
                        levels = rev(paste(item1, dimension, sep = "__"))),
         dimension = factor(paste0("Dimension ", dimension),
                            levels = paste0("Dimension ", as.factor(1:24)))) %>%
  ggplot(aes(item1, value, fill = dimension)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 4) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  labs(x = NULL, y = "Value",
       title = "First 24 principal components of the Hacker News corpus",
       subtitle = "Top words contributing to the components that explain the most variation")



