
library(tidyverse)
library(tidytext)
library(ggthemes)
library(scales)
library(DT)
library(stm)
library(furrr)
library(ggraph)
library(igraph)

# from https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R

reorder_within <- function (x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

load("cleanData.Rdata")

# allow larger uploads

options(shiny.maxRequestSize = 100 * 1024 ^ 2) 

function(input, output) {
  
  # data selection----
  
  dataReturn <- reactive({
    
    # set values
    
    document_prop = input$documentProportion
    trim_common = input$trimCommon
    trim_rare = input$trimRare
    
    use_data2 <- use_data %>% 
      sample_frac(document_prop) %>% 
      select(comment, Improve)
    
    # unedited tidy words
    
    tidy_stm <- use_data2 %>%
      unnest_tokens(word, Improve) %>%
      anti_join(stop_words)
    
    # this is me trimming *common* words
    
    if(trim_common){
      
      counts_common <- use_data2 %>% 
        unnest_tokens(word, Improve) %>% 
        anti_join(stop_words) %>% 
        count(word, sort = TRUE) %>% 
        top_n(trim_common)
      
      tidy_stm <- tidy_stm %>% 
        anti_join(counts_common)
    }
    
    if(trim_rare){
      
      counts_rare <- use_data2 %>% 
        unnest_tokens(word, Improve) %>% 
        anti_join(stop_words) %>% 
        count(word, sort = TRUE) %>% 
        filter(n <= trim_rare)
      
      if(input$keepReal) {
        counts_rare <- counts_rare %>% 
          anti_join(get_sentiments("bing"))
      }
      
      tidy_stm <- tidy_stm %>% 
        anti_join(counts_rare)
    }
    
    return(tidy_stm)
  })
  
  # control to select number of topics
  
  output$numberOfTopics <- renderUI({
    
    req(topicModels())
    
    selectInput("selectNumberTopics", "Select number of topics", choices = topicModels()[["many_models"]]$K)
  })
  
  # control to select individual topics
  
  output$selectTopic <- renderUI({
    
    req(topicModels())
    
    numericInput("selectIndividualTopic", "Select individual topic", 
                 value = 1, min = 1, 
                 max = topicModels()[["many_models"]]$K,
                 step = 1)
  })
  
  # show details of matrix
  
  output$sparseMatrix <- renderText({
    
    tidy_sparse <- dataReturn() %>%
      count(comment, word, sort = TRUE) %>%
      cast_sparse(comment, word, n)
    
    paste0(tidy_sparse@Dim[1], " documents and a ", tidy_sparse@Dim[2], " vocabulary")
  })
  
  output$showData <- renderDT({
    
    # set up a dependency with the stm code
    
    do_nothing = topicModels()
    
    dataReturn() %>%
      count(word, sort = TRUE)
  })
  
  # model fit and diagnostics----
  
  topicModels <- reactive({
    
    if(!is.null(input$modelFile)){
      
      the_models <- readRDS(input$modelFile$datapath)
      
      return(the_models)
    }
    
    if(input$executeModel == 0){
      
      return() 
    }
    
    showModal(modalDialog("Calculating... This will take a while...", footer = NULL))
    
    tidy_sparse <- dataReturn() %>%
      count(comment, word, sort = TRUE) %>%
      cast_sparse(comment, word, n)
    
    plan(multiprocess)
    
    many_models <- data_frame(K = c(30, 40)) %>%
    # many_models <- data_frame(K = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 125, 150)) %>%
      # mutate(topic_model = future_map(K, ~stm(tidy_sparse, K = .,
      mutate(topic_model = map(K, ~stm(tidy_sparse, K = .,
                                              verbose = FALSE)))
    
    removeModal()
    
    heldout <- make.heldout(tidy_sparse)
    
    k_result <- many_models %>%
      mutate(exclusivity = map(topic_model, exclusivity),
             semantic_coherence = map(topic_model, semanticCoherence, tidy_sparse),
             eval_heldout = map(topic_model, eval.heldout, heldout$missing),
             residual = map(topic_model, checkResiduals, tidy_sparse),
             bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
             lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
             lbound = bound + lfact,
             iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
    
    saveRDS(list("many_models" = many_models, "tidy_sparse" = tidy_sparse, "k_result" = k_result), 
            file = paste0(Sys.Date(), "_prop-", input$documentProportion,
                          "_trim_common-", input$trimCommon,
                          "_trim_rare-", input$trimRare, ".rds"))
    
    return(list("many_models" = many_models, "tidy_sparse" = tidy_sparse, 
                "k_result" = k_result))
  })
  
  output$modelDiagnostics <- renderPlot({
    
    validate(
      need(topicModels()[["k_result"]], "Please run a topic model first")
    )
    
    topicModels()[["k_result"]] %>%
      transmute(K,
                `Lower bound` = lbound,
                Residuals = map_dbl(residual, "dispersion"),
                `Semantic coherence` = map_dbl(semantic_coherence, mean),
                `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
      gather(Metric, Value, -K) %>%
      ggplot(aes(K, Value, color = Metric)) +
      geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
      facet_wrap(~Metric, scales = "free_y") +
      labs(x = "K (number of topics)",
           y = NULL,
           title = "Model diagnostics by number of topics")
  })
  
  output$betaPlot <- renderPlot({
    
    validate(
      need(topicModels()[["k_result"]], "Please run a topic model first")
    )
    
    topic_model <- topicModels()[["k_result"]] %>% 
      filter(K == input$selectNumberTopics) %>% 
      pull(topic_model) %>% 
      .[[1]]
    
    td_beta <- tidy(topic_model)
    
    # plots with high scoring words per topic
    
    td_beta %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      mutate(topic = paste0("Topic ", topic),
             term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = as.factor(topic))) +
      geom_col(alpha = 0.8, show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free_y") +
      coord_flip() +
      scale_x_reordered() +
      labs(x = NULL, y = expression(beta),
           title = "Highest word probabilities for each topic",
           subtitle = "Different words are associated with different topics")
  })
  
  output$gammaPlot <- renderPlot({
    
    topic_model <- topicModels()[["k_result"]] %>% 
      filter(K == input$selectNumberTopics) %>% 
      pull(topic_model) %>% 
      .[[1]]
    
    td_beta <- tidy(topic_model)
    
    tidy_sparse <- topicModels()[["tidy_sparse"]]
    
    td_gamma <- tidy(topic_model, matrix = "gamma",
                     document_names = rownames(tidy_sparse))
    
    top_terms <- td_beta %>%
      arrange(beta) %>%
      group_by(topic) %>%
      top_n(7, beta) %>%
      arrange(-beta) %>%
      select(topic, term) %>%
      summarise(terms = list(term)) %>%
      mutate(terms = map(terms, paste, collapse = ", ")) %>% 
      unnest()
    
    gamma_terms <- td_gamma %>%
      group_by(topic) %>%
      summarise(gamma = mean(gamma)) %>%
      arrange(desc(gamma)) %>%
      left_join(top_terms, by = "topic") %>%
      mutate(topic = paste0("Topic ", topic),
             topic = reorder(topic, gamma))
    
    gamma_terms %>%
      top_n(20, gamma) %>%
      ggplot(aes(topic, gamma, label = terms, fill = topic)) +
      geom_col(show.legend = FALSE) +
      geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
                family = "IBMPlexSans") +
      coord_flip() +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0, 0.09),
                         labels = percent_format()) +
      theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
      theme(plot.title = element_text(size = 16,
                                      family="IBMPlexSans-Bold"),
            plot.subtitle = element_text(size = 13)) +
      labs(x = NULL, y = expression(gamma),
           title = "Top 20 topics by prevalence",
           subtitle = "With the top words that contribute to each topic")
  })
  
  output$topDocuments <- renderText({
    
    topic_model <- topicModels()[["k_result"]] %>% 
      filter(K == input$selectNumberTopics) %>% 
      pull(topic_model) %>% 
      .[[1]]
    
    td_beta <- tidy(topic_model)
    
    tidy_sparse <- topicModels()[["tidy_sparse"]]
    
    td_gamma <- tidy(topic_model, matrix = "gamma",
                     document_names = rownames(tidy_sparse))
    
    top_terms <- td_beta %>%
      arrange(beta) %>%
      group_by(topic) %>%
      top_n(7, beta) %>%
      arrange(-beta) %>%
      select(topic, term) %>%
      summarise(terms = list(term)) %>%
      mutate(terms = map(terms, paste, collapse = ", ")) %>% 
      unnest()
    
    gamma_terms <- td_gamma %>%
      group_by(topic) %>%
      summarise(gamma = mean(gamma)) %>%
      left_join(top_terms, by = "topic")
    
    gamma_documents <- td_gamma %>% 
      group_by(topic) %>% 
      top_n(10, gamma)
    
    map_chr(1 : nrow(gamma_terms), function(x){
      
      document_indices <- gamma_documents %>% 
        filter(topic == x) %>% 
        pull(document) %>% 
        as.numeric()
      
      paste0("<h2>", gamma_terms[x, "topic"], "</h2>",
             paste0("<p>", 
                    use_data %>% 
                      filter(comment %in% document_indices) %>% 
                      pull(Keep_Improve),
                    "</p>",
                    collapse = "")
      )
    })
  })
  
  output$documentGamma <- renderPlot({
    
    topic_model <- topicModels()[["k_result"]] %>% 
      filter(K == input$selectNumberTopics) %>% 
      pull(topic_model) %>% 
      .[[1]]
    
    td_beta <- tidy(topic_model)
    
    tidy_sparse <- the_models[["tidy_sparse"]]
    
    td_gamma <- tidy(topic_model, matrix = "gamma",
                     document_names = rownames(tidy_sparse))
    
    td_gamma %>% 
      group_by(topic) %>% 
      top_n(10, gamma) %>% 
      ggplot(aes(x = gamma)) + geom_histogram() +
      facet_wrap(~ topic)
  })
  
  output$bigramNetwork <- renderPlot({
    
    topic_model <- topicModels()[["k_result"]] %>% 
      filter(K == input$selectNumberTopics) %>% 
      pull(topic_model) %>% 
      .[[1]]
    
    td_beta <- tidy(topic_model)
    
    tidy_sparse <- the_models[["tidy_sparse"]]
    
    td_gamma <- tidy(topic_model, matrix = "gamma",
                     document_names = rownames(tidy_sparse))
    
    gamma_documents <- td_gamma %>% 
      group_by(topic) %>% 
      top_n(100, gamma)
    
    select_documents <- gamma_documents %>% 
      filter(topic == input$selectIndividualTopic) %>% 
      pull(document)
    
    bigrams <- use_data %>% 
      filter(comment %in% select_documents) %>%  
      select(Improve) %>% 
      unnest_tokens(bigram, Improve, token = "ngrams", n = 2)
    
    bigrams_separated <- bigrams %>% 
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered <- bigrams_separated %>% 
      filter(!word1 %in% stop_words$word) %>% 
      filter(!word2 %in% stop_words$word) %>% 
      filter(!grepl('^\\d+$', word1)) %>% 
      filter(!grepl('^\\d+$', word2))
    
    bigram_counts <- bigrams_filtered %>% 
      count(word1, word2, sort = TRUE)
    
    bigram_graph <- bigram_counts %>% 
      arrange(-n) %>% 
      head(input$bigramSlider) %>%
      graph_from_data_frame()
    
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), arrow = arrow()) + 
      geom_node_point() +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  })
}


