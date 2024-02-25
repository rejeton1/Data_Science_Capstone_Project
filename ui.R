#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Smart Note Pad"),
  
  textAreaInput(inputId="area1", label="(Type here!)"),
  
  hr(),
  
  fluidRow(
    column(4,
           actionButton("button1", label=textOutput("word1"))
           ),
    column(4,
           actionButton("button2", label=textOutput("word2"))
           ),
    column(4,
           actionButton("button3", label=textOutput("word3"))
           )
  )
))