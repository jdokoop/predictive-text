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
  # The output list will contain (1) the prediction for the next word in the sequence
  # and (2) all of the most likely candidate words in a data frame
  outList <- list()
  
  # Since this will be reactive, do nothing if user hasn't yet typed anything
  if(s == "")
  {
    outList[[1]] <- ""
    return(outList)
  }
  
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
    topWord <- paste(s, tetragramCandidates[which.max(tetragramCandidates$cnt), c("fourthWord")], sep = " ")
    outList[[1]] <- topWord
    outList[[2]] <- tetragramCandidates[, c("fourthWord","cnt")]
    outList[[2]] <- outList[[2]][order(-outList[[2]]$cnt), ]
    colnames(outList[[2]]) <- c("word", "cnt")
    return(outList)      
  }
  else if(nrow(trigramCandidates) > 0)
  {
    topWord <- paste(s, trigramCandidates[which.max(trigramCandidates$cnt), c("thirdWord")], sep = " ")
    outList[[1]] <- topWord
    outList[[2]] <- trigramCandidates[, c("thirdWord","cnt")]
    colnames(outList[[2]]) <- c("word", "cnt")
    return(outList)
  }
  else if(nrow(bigramCandidates) > 0)
  {
    topWord <- paste(s, bigramCandidates[which.max(bigramCandidates$cnt), c("secondWord")], sep = " ")
    outList[[1]] <- topWord
    outList[[2]] <- bigramCandidates[, c("secondWord","cnt")]
    colnames(outList[[2]]) <- c("word", "cnt")
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
  
  # Run prediction function, which returns a list
  # The first item in the list is the prediction
  # The second item is a dataframe with the most likely words to complete the sentence
  getPrediction <- reactive({predictNextWord(input$inputSentence)})
  
  # Use the first entry in the list to display the input sentence with the predicted next word
  output$outputSentence <- renderText({ 
    getPrediction()[[1]]
  })
  
  # Use the second entry in the list to display a histogram of the most likely completions
  output$distPlot <- renderPlot({
    par(las=2)
    barplot(getPrediction()[[2]][,2], names.arg = getPrediction()[[2]][,1], ylab="Probability")
  })
  
})