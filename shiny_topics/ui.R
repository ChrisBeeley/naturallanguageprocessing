
library(DT)
library(plotly)

fluidPage(
  
  titlePanel("Tuning stm models"),
  
  sidebarLayout(
    div(id ="Sidebar", sidebarPanel(
      
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
      
      uiOutput("numberOfTopics"),
      
      uiOutput("selectTopic")
    )
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
          plotOutput("betaPlot", height = "800px")
        ),
        tabPanel(
          "Gamma plot",
          plotOutput("gammaPlot")
        ),
        tabPanel(
          "Top documents",
          p("Top 100 documents for each topic."),
          htmlOutput("topDocuments")
        ),
        tabPanel(
          "Document gamma distribution",
          p("Check distribution of p"),
          plotOutput("documentGamma", height = "800px")
        ),
        tabPanel(
          "Tag network",
          plotOutput("bigramNetwork"),
          sliderInput("bigramSlider", "Number of terms", 20, 160, 100, step = 10)
        )
      )
    )
  )
)
