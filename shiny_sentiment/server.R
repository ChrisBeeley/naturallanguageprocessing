
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)

load("../cleanData.Rdata")

function(input, output) {
   
  output$bigrams <- renderPlot({
    
    bigrams <- use_data %>% 
      select(Improve, Division2) %>% 
      unnest_tokens(bigram, Improve, token = "ngrams", n = 2)
    
    bigrams_separated <- bigrams %>% 
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered <- bigrams_separated %>% 
      filter(!word1 %in% stop_words$word) %>% 
      filter(!word2 %in% stop_words$word)
    
    bigram_counts <- bigrams_filtered %>% 
      count(word1, word2, sort = TRUE)
    
    bigram_graph <- bigram_counts %>% 
      filter(n > 20) %>% 
      graph_from_data_frame()
    
    # set.seed(1234)
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link() + 
      geom_node_point() +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1)

  })
  
}

