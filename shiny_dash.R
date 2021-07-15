library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(wordcloud2)
library(tm)
library(colourpicker)


ui<-dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Fake And True News Dataset ",titleWidth = 300),
  dashboardSidebar(sidebarMenu(
    fileInput("upload","Upload the Dataset"),
    menuItem("Dataset",tabName = "dataset", icon = icon("book")),
    menuItem("Plots", tabName = "plots", icon = icon("chart-bar")),
    menuItem("Word cloud", tabName = "word_cloud", icon = icon("certificate")),
    radioButtons(
      inputId = "source",
      label = "Word source",
      choices = c(
        "Fake news and True news" = "news",
        "Fake news" = "fake",
        "True news" = "true",
        "Use your own words" = "own",
        "Upload a file" = "file"
      )
    ),
    selectInput(
      inputId = "language",
      label = "Remove stopwords in",
      choices = c( "English", "Hindi", "malayalam"),
      multiple = FALSE,
      selected = "English"
    ),
    conditionalPanel(
      condition = "input.source == 'own'",
      textAreaInput("text", "Enter text", rows = 7)
    ),
    # Wrap the file input in a conditional panel
    conditionalPanel(
      # The condition should be that the user selects
      # "file" from the radio buttons
      condition = "input.source == 'file'",
      fileInput("file", "Select a file")
    ),
    hr(),
    checkboxInput("remove_words", "Remove specific words?", FALSE),
    conditionalPanel(
      condition = "input.remove_words == 1",
      textAreaInput("words_to_remove1", "Words to remove (one per line)", rows = 1)
    ),
    conditionalPanel(
      condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
      textAreaInput("words_to_remove2", "", rows = 1)
    ),
    conditionalPanel(
      condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
      textAreaInput("words_to_remove3", "", rows = 1)
    ),
    conditionalPanel(
      condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
      textAreaInput("words_to_remove4", "", rows = 1)
    ),
    conditionalPanel(
      condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
      textAreaInput("words_to_remove5", "", rows = 1)
    ),
    conditionalPanel(
      condition = "input.remove_words == 1 && input.words_to_remove5.length > 0",
      textAreaInput("words_to_remove6", "", rows = 1)
    ),
    conditionalPanel(
      condition = "input.remove_words == 1 && input.words_to_remove6.length > 0",
      textAreaInput("words_to_remove7", "", rows = 1)
    ),
    conditionalPanel(
      condition = "input.remove_words == 1 && input.words_to_remove7.length > 0",
      textAreaInput("words_to_remove8", "", rows = 1)
    ),
    conditionalPanel(
      condition = "input.remove_words == 1 && input.words_to_remove8.length > 0",
      textAreaInput("words_to_remove9", "", rows = 1)
    ),
    conditionalPanel(
      condition = "input.remove_words == 1 && input.words_to_remove9.length > 0",
      textAreaInput("words_to_remove10", "", rows = 1)
    ),
    hr(),
    numericInput("num", "Maximum number of words",
                 value = 100, min = 5
    ),
    hr(),
    colourInput("col", "Background color", value = "white")
     
  )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(
        h2(" News "),
        tabName = "dataset",
        mainPanel(width = 20,
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Dataset",DT::dataTableOutput("data1")),
                    tabPanel("Summary",verbatimTextOutput("summary"))
                    
                  )
        )
        
      ),
      tabItem(
        h2("Plots"),
        tabName = "plots",
        
        mainPanel(
          width = 20,
          type="tabs",
          tabPanel("barplot 1",plotOutput("plot")),
          tabPanel("barplot 2",plotOutput("plot2")),
          tabPanel("barplot 3",plotOutput("plot3"))
        )
      ),
      
      tabItem(
        h2("wordcloud"),
        tabName = "word_cloud",
        
        mainPanel(
          width = 20,
          type = "tabs",
          tabPanel("wordcloud1",wordcloud2Output("cloud") )
        )
      )
      
      
    )
  )
)



library(ggplot2)
library(reshape2)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(dlookr)
library(lattice)
library(corrplot)
library(ggcorrplot)
library(naniar)
library(skimr)
library(dplyr)
library(datasets)
library(wordcloud2)


options(shiny.maxRequestSize=150*1024^2)         

server<-function(input,output){
  
  dtset <- reactive({
    if(is.null(input$upload)){
      return(NULL)
    }
    read.csv(input$upload$datapath,header = TRUE,sep=",")
  })
  
  # NEWS DATASET 
  output$data1<-DT::renderDataTable({
    DT::datatable(dtset(),
                  options=list(scrollX=TRUE)
    )
  })
  
  output$summary<-renderPrint({
    summary(dtset())
  })
  
  #plots
  
  output$plot <- renderPlot({
    
    mpg <- read.csv("combined.csv", header = TRUE, sep = ",")
    
    
    #plot1
    
    
    mpg$category <- as.factor(mpg$category)
    
    plot1 <- ggplot(mpg, aes(x = category, fill = category)) + 
      geom_bar() +
      theme_classic() +
      theme(axis.title = element_text(face = 'bold', size = 15),
            axis.text = element_text(size = 13)) +
      theme(legend.position = 'none')
    plot1
    
  })
  
  output$plot2 <- renderPlot({
    
    mpg1 <- read.csv("combined.csv", header = TRUE, sep = ",")
    
    
    #plot2
    
    plot2 <- ggplot(mpg1, aes(x = subject, fill = subject)) + 
      geom_bar() +
      theme_classic() +
      theme(axis.title = element_text(face = 'bold', size = 15),
            axis.text = element_text(size = 13)) +
      theme(legend.position = 'none')
    plot2
    
    
  })
  
  output$plot3 <- renderPlot({
    
    mpg2 <- read.csv("combined.csv", header = TRUE, sep = ",")
    
    
    #plot3
    
    plot3 <- ggplot(mpg2, aes(x = subject, fill = category)) +
      geom_bar(position = 'dodge', alpha = 0.6) +
      theme_classic() +
      theme(axis.title = element_text(face = 'bold', size = 15),
            axis.text = element_text(size = 13, angle = 90))
    
    plot3
    
    
  })
  
  #wordcloud
  
  data_source <- reactive({
    if (input$source == "news") {
      data <- read.csv("fake_true_news.csv",
                       sep = "&",
                       stringsAsFactors = FALSE
      )
      data <- data[, 1]
    }else if (input$source == "fake"){
      data <- read.csv("fake.csv", sep = "&", stringsAsFactors = FALSE)
      data <- data[, 1]
    }else if (input$source == "true"){
      data <- read.csv("true.csv", sep = "&", stringsAsFactors = FALSE, row.names = NULL)
      data <- data[, 1]
    }else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  create_wordcloud <- function(data, num_words = 100, background = "white") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    }
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }
   wordcloud2(data, backgroundColor = background)
   
  }
    
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })  
  
  


}

shinyApp(ui,server)