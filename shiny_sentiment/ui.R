fluidPage(
  
  # Application title
  titlePanel("Text analysis of Notts HC patient experience"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("bigrams")
    )
  )
)