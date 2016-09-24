library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(img(src="header.png", height = 130, width = 900)),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       h5("Start by typing a sentence in the box below. The prediction for the next word will appear.
         The graph on the right will show the top words most likely to complete your sentence. Please
          allow 5 seconds for the model the load."),
       
       h5("__________________________________"),
    
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
