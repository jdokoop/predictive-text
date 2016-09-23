#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Load text prediction ngram model from csv file
model <- read.csv("myModelStepFinal.0.csv")

# Function to predict next word in sentence fragment passed as argument
predictNextWord <- function(s)
{
  print("Calling prediction function...")
  print(s)
  # Since this will be reactive, do nothing if user hasn't yet typed anything
  if(s == "")
  {
    outList <- list()
    outList[[1]] <- ""
    return(outList)
  }
  
  print("Predicting for non-null sentence")
  
  # TODO: Apply same transformations to sentence as to training corpus
  
  # Split sentence into words 
  tokenizedSentence = strsplit(s, " ")[[1]]
  
  # Separate the n-grams into different tables
  modelBigrams    <- model[which(model$thirdWord=='^' & model$fourthWord=='^'),]
  modelTrigrams   <- model[which(model$thirdWord!='^' & model$fourthWord=='^'),]
  modelTetragrams <- model[which(model$fourthWord!='^'),]
  
  # Check if there is a known tetragram starting with the last three words of the sentence
  tetragramCandidates <- modelTetragrams[which(modelTetragrams$firstWord==tokenizedSentence[1] & modelTetragrams$secondWord==tokenizedSentence[2] & modelTetragrams$thirdWord==tokenizedSentence[3]),]
  trigramCandidates    <- modelTrigrams[which(modelTrigrams$firstWord==tokenizedSentence[2] & modelTrigrams$secondWord==tokenizedSentence[3] & modelTrigrams$thirdWord!="^"),]
  bigramCandidates    <- modelBigrams[which(modelBigrams$firstWord==tokenizedSentence[3] & modelBigrams$secondWord!="^"),]
  
  if(nrow(tetragramCandidates) > 0)
  {
    topWord <- paste(s, tetragramCandidates[which.max(tetragramCandidates$cnt), "fourthWord"], sep = " ")
    outList <- list()
    outList[[1]] <- topWord
    #outList[[2]] <- tetragramCandidates[, "fourthWord"]
    return(outList)      
  }
  else if(nrow(trigramCandidates) > 0)
  {
    topWord <- paste(s, trigramCandidates[which.max(trigramCandidates$cnt), "thirdWord"], sep = " ")
    outList <- list()
    outList[[1]] <- topWord
    return(outList)
  }
  else if(nrow(bigramCandidates) > 0)
  {
    topWord <- paste(s, bigramCandidates[which.max(bigramCandidates$cnt), "secondWord"], sep = " ")
    outList <- list()
    outList[[1]] <- topWord
    return(outList)
  }
  else
  {
    outList <- list()
    outList[[1]] <- "NO COMPLETION FOUND"
    return(outList)
  }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  # Run prediction function, which returns a list
  # The first item in the list is the prediction
  # The second item is a dataframe with the most likely words to complete the sentence

  getPrediction <- reactive({predictNextWord(input$inputSentence)})
  
  output$outputSentence <- renderText({ 
    getPrediction()[[1]]
  })
  
})