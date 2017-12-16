library(shiny)
library(ggplot2)
library(readr)

movies <- read_csv("movie_metadata.csv")
dataset <- diamonds
option1 <- c("Visualization","Random Forest")
visOpt <- c("Mean Ratings")

fluidPage(
  
  titlePanel("Movie Predictions"),
  sidebarPanel(
    selectInput('options', 'Options', option1),
    selectInput('visOpts', "Select if Option chosen was Visualization: Visualization Options", visOpt ),
    textOutput("info")
  ),
  
  mainPanel(
    
    verticalLayout( 
      plotOutput('plot')
      
    )
  )
)