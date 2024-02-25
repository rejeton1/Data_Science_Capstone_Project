#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


source(url("https://raw.githubusercontent.com/rejeton1/Data_Science_Capstone_Project/main/LaunchMarkov.R"))


shinyServer(function(input, output, session) {
  
  pred <- reactive({
    pred_with_keyword_matching(input$area1)
  })
    
  output$word1 <- renderText(as.character(pred()$lastTerm[1]))
  output$word2 <- renderText(as.character(pred()$lastTerm[2]))
  output$word3 <- renderText(as.character(pred()$lastTerm[3]))

  
  text <- reactive({
    unlist(str_split(input$area1, "\\s"))
  })
  
  new_text1 <- reactive({
    paste(str_trim(paste(paste(text()[1:length(text())-1], collapse = " "), pred()$lastTerm[1])), "")
  })
  new_text2 <- reactive({
    paste(str_trim(paste(paste(text()[1:length(text())-1], collapse = " "), pred()$lastTerm[2])), "")
  })
  new_text3 <- reactive({
    paste(str_trim(paste(paste(text()[1:length(text())-1], collapse = " "), pred()$lastTerm[3])), "")
  })
  

  observeEvent(input$button1, {
    updateTextAreaInput(session, inputId = 'area1', 
                        value=new_text1())
  })
  
  observeEvent(input$button2, {
    updateTextAreaInput(session, inputId = 'area1', 
                        value=new_text2())
  })
  
  observeEvent(input$button3, {
    updateTextAreaInput(session, inputId = 'area1', 
                        value=new_text3())
  })
  
})
