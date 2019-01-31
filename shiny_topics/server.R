
library(tidyverse)
library(tidytext)
library(ggthemes)
library(scales)
library(DT)
library(stm)
library(furrr)

# from https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R

reorder_within <- function (x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

load("cleanData.Rdata")

# allow larger uploads

options(shiny.maxRequestSize = 60 * 1024 ^ 2) 

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
    
    # many_models <- data_frame(K = c(20, 40)) %>%
    many_models <- data_frame(K = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 125, 150)) %>%
      mutate(topic_model = future_map(K, ~stm(tidy_sparse, K = .,
                                              verbose = FALSE)))
    
    saveRDS(list("many_models" = many_models, "tidy_sparse" = tidy_sparse), 
            file = paste0(Sys.Date(), "_prop-", input$documentProportion,
                          "_trim_common-", input$trimCommon,
                          "_trim_rare-", input$trimRare, ".rds"))
    
    removeModal()
    
    return(list("many_models" = many_models, "tidy_sparse" = tidy_sparse))
  })
  
  output$modelDiagnostics <- renderPlot({
    
    validate(
      need(topicModels()[["many_models"]], "Please run a topic model first")
    )
    
    tidy_sparse <- topicModels()[["tidy_sparse"]]
    
    many_models <- topicModels()[["many_models"]]
    
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
    
    k_result %>%
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
    
    
  })

    
}


