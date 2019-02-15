
fluidPage(
  
  titlePanel("Explore doc2vec generated similar documents"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("fetchNewComments", "Fetch new comments")    
    ),
    
    mainPanel(
      htmlOutput("similarComments")
    )
  )
)