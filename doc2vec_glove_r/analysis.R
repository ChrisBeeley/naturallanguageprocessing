
library(text2vec)
library(Rtsne)
library(tidyverse)
library(plotly)
library(dbscan)
library(tidytext)

load("cleanData.Rdata")

wiki = use_data$Improve

### this is a setup that works well for unigrams- don't mess it up!----

# Create iterator over tokens

tokens = space_tokenizer(wiki)

# Create vocabulary. Terms will be unigrams (simple words)

it = itoken(tokens, progressbar = FALSE)

# select 1 or 2 grams

vocab = create_vocabulary(it)

# vocab = create_vocabulary(it, c(2, 2), stopwords = stop_words$word)

vocab = prune_vocabulary(vocab, term_count_min = 20L)

# Use our filtered vocabulary
vectorizer = vocab_vectorizer(vocab)

# use window of 5 for context words
tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)

# fit model
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 200, convergence_tol = 0.01)

wv_context = glove$components

word_vectors = wv_main + t(wv_context)

apply(word_vectors, 1, sum)
