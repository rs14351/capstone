suppressWarnings(library(shiny))

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict Next Word"),
  
  fluidRow(HTML("<strong>Author: Sharon Ng</strong>") ),
  fluidRow(HTML("<strong>Date: 20-Jan-2016</strong>") ),
  
  fluidRow(
    br(),
    p("This Shiny application uses N-Gram Back Off model to predict tne next word. The sample sentences came from three English data sources: twitter, news and blogs data. This data was cleaned and processed to create data frames of quad, tri, bi and unigram(s) with sorted cumulative frequencies. 
      The Shiny app loads the four saved n-gram objects and apply a simple Katz's Back-off algorithm to predict the next word after user enters a partial sentence.")),
  br(),
  br(),
  
  fluidRow(HTML("<strong>Enter your sentence. Press \"Next Word\" button to predict the next word</strong>") ),
  fluidRow( p("\n") ),
  
  # Sidebar layout
  sidebarLayout(
    
    sidebarPanel(
      textInput("inputString", "Enter a partial sentence here",value = ""),
      submitButton("Next Word")
    ),
    
    mainPanel(
      h4("Predicted Next Word"),
      verbatimTextOutput("prediction"),
      textOutput('text1'),
      textOutput('text2')
    )
  )
    ))