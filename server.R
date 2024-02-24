#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

source("https://raw.githubusercontent.com/rejeton1/Data_Science_Capstone_Project/main/LaunchMarkov.R")


shinyServer(function(input, output, session) {
  
  pred <- reactive({
    pred_with_keyword_matching(input$area1)
  })
  
  text <- reactive({
    str_split(input$area1, "\\s")
  })
    
  output$word1 <- renderText(as.character(pred()$lastTerm[1]))
  output$word2 <- renderText(as.character(pred()$lastTerm[2]))
  output$word3 <- renderText(as.character(pred()$lastTerm[3]))

  
})
