#
# #' ## Shiny App 
#' This script creates a Shiny App that takes a word or phrase input in a text box
#' and outputs the a predicted next word. 

library(shiny)
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

#' Source ngram matching function
source("N_gram_code.R")

#' Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("WORD PREDICTION"),
  p("Enter a word or words and get outcome of next word for a sentense"),
  sidebarLayout(
    sidebarPanel(
      h3("INSTRUCTIONS:"),
      h5("1. Enter a word or words in the entry box"),
      h5("2. The next word predicted shows below entry box in red"),
      h5("3. A question mark shown means no prediction word")),
    mainPanel(
      textInput("user_input", h3("Your Input:"),
                value = "Enter words here"),
      h3("Next Word Predicted:"),
      h5(em(span(textOutput("N_gram_code_output"))))
    )
  ) 
)
server <- function(input, output) {
  output$N_gram_code_output <- renderText({
    ngrams(input$user_input)
  })
}
shinyApp(ui=ui,server = server)