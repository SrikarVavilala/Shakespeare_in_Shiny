### SRIKAR VAVILALA

# import libs
library(shiny)
library(tidytext)
library(RColorBrewer)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(wordcloud)

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}

# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel (done)
  
  # task2: add in the inputs in the sidebarPanel
  
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights
  
  sidebarLayout(
    
    sidebarPanel(
      
      # drop down box for choosing book
      selectInput(inputId = "book_name", label = "Choose a book:",
                  choices = books,
                  selected = "A Mid Summer Night's Dream"),
      
      # checkbox for whether or not to remove stopwords
      checkboxInput(inputId = "stopwords", label = "Stop words:",
                    value = TRUE),
      
      # button to rerun 
      actionButton(inputId = "rerun_button", 
                   label = "Rerun"),
      
      # horizontal bar to separate sections of sidepanel
      hr(),
      
      # header to denote that the next 3 inputs are related to the Word Cloud tab.
      h3("Word Cloud Settings"),
      
      # slider input for setting max. num. of words in Word Cloud
      sliderInput(inputId = "maxwords", label = "Max # of Words",
                  min = 10, max = 200, value = 100, step = 10),
      
      # slider input for size of largest words
      sliderInput(inputId = "word_size_large", label = "Size of largest words:",
                  min = 1, max = 8, value = 4),
      
      # slider input for size of smallest words
      sliderInput(inputId = "word_size_small", label = "Size of smallest words:",
                  min = 0.1, max = 4, value = 0.5),
      
      # horizontal bar to denote the Word Counts chart parameters 
      hr(),
      
      # header to denote that the next inputs are related to the Word Count tab.
      h3("Word Count Settings"),
      
      # slider input for minimum word counts for words to show up in the Word Counts chart
      sliderInput(inputId = "min_word_counts", label = "Minimum words for Counts Chart:",
                  min = 10, max = 100, value = 25),
      
      # slider input for font size of words in the Word Counts chart
      sliderInput(inputId = "font_counts", label = "Word size for Counts Chart:",
                  min = 8, max = 30, value = 14),
      
    ),
    
    mainPanel(
      
      # tabs for Word Cloud and Word Counts
      tabsetPanel(type="tabs", 
                  tabPanel(title = "Word Cloud",
                           # plot space for word cloud tab
                           plotOutput(outputId = "cloud", height = "600px", width = "600px")),
                  tabPanel(title = "Word Counts",
                           # plot space for word count tab
                           plotOutput(outputId = "freq_counts", height = "600px", width = "600px"))
                  )
    )
    
  )
  
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$rerun_button,
                             {withProgress({
                               setProgress(message = "Processing corpus...")
                               getFreq(input$book_name, input$stopwords)
                             })
                               }
                             )
  
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$word_size_large, input$word_size_small),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
    })
  
  
  output$freq_counts <- renderPlot({
    
    v <- freq() 
    v %>% filter(n > input$min_word_counts) %>%
    ggplot(aes(x = reorder(word,n), y = n))+
             geom_bar(stat='identity')+
             coord_flip()+
             theme(text=element_text(size = 8),
                   axis.text.x=element_text(size = input$font_counts),
                   axis.text.y=element_text(size = input$font_counts),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
                   
           
                        
    
    
  })
  
}

shinyApp(ui = ui, server = server)
