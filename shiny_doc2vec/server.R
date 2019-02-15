
library(tidyverse)

df = read_csv("document_results.csv")

df <- df %>% 
  rename(row_id = X1)

df$similar_vector <- gsub("[[", "", df$similar_vector, fixed = TRUE)

df$similar_vector <- gsub("]]", "", df$similar_vector, fixed = TRUE)

df$similar_vector = strsplit(df$similar_vector, ",")

final_df <- df %>% 
  cbind(
    new_df <- map(1 : 10, function(y){
      
      map_dbl(df$similar_vector, function(x) as.numeric(x[[y]]))
      
    }) %>% as_tibble(.name_repair = "unique")
  )

function(input, output) {
  
  output$similarComments <- renderText({
    
    input$fetchNewComments
    
    indices <- sample(1 : nrow(final_df), 10, replace = FALSE) + 1
    
    comments <- final_df$Keep_Improve[indices]
    
    list_output <- map(indices, function(i) {
      final_df %>% 
        select(num_range("..", 1 : 10)) %>% 
        slice(i) %>% 
        unlist() %>% 
        map(function(x) {
          
          print(final_df$Keep_Improve[[x]])
        })
    }) %>% 
      as_tibble(.name_repair = "unique")
    
    similar_comments <- lapply(list_output, function (x){
      
      paste0("<p>", unlist(x), "</p>", collapse = "")
    })
    
    return(paste("<h2>", comments, "</h2>", similar_comments))
    
  })
}
    
    