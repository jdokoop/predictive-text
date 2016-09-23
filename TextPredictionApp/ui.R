#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(img(src="header.png", height = 130, width = 900)),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       h5("Start by typing a sentence in the box below. The prediction for the next word will appear below.
         The graph on the right will show the top words most likely to complete your sentence."),
       
       h5(" "),
    
       h5("Start Typing Here"),
      
       textInput("inputSentence", NULL, value = "", width = 500),
      
       h5("Did You Mean...?"),
      
       textOutput("outputSentence", tags$textarea)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
