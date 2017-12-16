library(shiny)
  
  ui <- fluidPage(
    titlePanel("Movie Success Prediction "),
    textInput("txt1", "Enter moviename"),
    textInput("txt2", "Enter Actor 1"),
    textInput("txt3", "Enter Actor 2"),
    textInput("txt4", "Enter Actor 3"),
    textInput("txt5", "Enter director name"),
    textInput("txt6", "Enter number of faces on the poster"),
    textInput("txt7", "Enter the year"),
    textInput("txt8", "Enter the duration of the movie"),
    textInput("txt9", "Budget"),
    verbatimTextOutput("results"),
    actionButton("submit", "Ok"),
    actionButton("clear", "Reset")
  )


  
  