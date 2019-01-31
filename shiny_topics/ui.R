
library(DT)

fluidPage(
  
  titlePanel("Tuning stm models"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput("modelFile", "Load previous run",
                multiple = FALSE),
      
      sliderInput("documentProportion",
                  "Proportion of documents",
                  min = 0,
                  max = 1,
                  value = 1,
                  step = 0.1),
      
      sliderInput("trimCommon",
                  "Trim top n words",
                  min = 0,
                  max = 10,
                  value = 0,
                  step = 1),
      
      sliderInput("trimRare",
                  "Trim words with fewer than n occurences",
                  min = 0,
                  max = 20,
                  value = 0,
                  step = 1),
      
      checkboxInput("keepReal", HTML("When removing rare words keep real words using a lexicon")),
      
      actionButton("executeModel", "Run topic model"),
      
      uiOutput("numberOfTopics")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data selection",
          textOutput("sparseMatrix"),
          DTOutput("showData")
        ),
        tabPanel(
          "Model selection",
          plotOutput("modelDiagnostics")
        ),
        tabPanel(
          "Beta plot",
          plotOutput("betaPlot")
        ),
        tabPanel(
          "Gamma plot",
          p("This will hold a gamma plot")
        ),
        tabPanel(
          "Tag networks",
          p("Tab 5, app 1- diagnostics. Top 100 documents for each topic. Check distribution of p. Network graph of TAGS. ? network of words")
        )
      )
    )
  )
)
