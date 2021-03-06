library(shiny)
library(tm)

# Load text prediction ngram model from csv file
model <- read.csv("myModelProfanityRemoved.5.csv")

# Separate the n-grams into different tables
modelBigrams    <- model[which(model$thirdWord == '^' & model$fourthWord == '^'),]
modelTrigrams   <- model[which(model$thirdWord != '^' & model$fourthWord == '^'),]
modelTetragrams <- model[which(model$fourthWord != '^' & model$fifthWord == '^'),]
modelFivegrams  <- model[which(model$fifthWord != '^'),]

# Function to predict next word in sentence fragment passed as argument
predictNextWord <- function(s)
{
  # The output list will contain (1) the prediction for the next word in the sequence
  # and (2) all of the most likely candidate words in a data frame
  outList <- list()
  
  # Since this will be reactive, do nothing if user hasn't yet typed anything
  if(s == "")
  {
    word <- c("")
    cnt  <- c(0)
    
    outList[[1]] <- ""
    outList[[2]] <- data.frame(word,cnt)
    return(outList)
  }
  
  # Transform input sentence to lower case, remove punctuation, and apply other transformations to standardize the input
  corpus <- Corpus(VectorSource(s))
  
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1) 
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("it's", "it is", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("i'm", "i am", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("isn't", "is not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("can't", "cannot", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("won't", "will not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("don't", "do not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("doesn't", "does not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("didn't", "did not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("wouldn't", "would not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("shouldn't", "should not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("couldn't", "could not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("i've", "i have", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("we've", "we have", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("they've", "they have", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("i'll", "i will", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("you'll", "you will", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("they'll", "they will", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("we'll", "we will", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("can't", "cannot", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("he's", "he is", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("she's", "she is", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("we're", "we are", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("they're", "they are", x)))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("[^[:alnum:] ]", "", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("^\\s+|\\s+$", "", x)))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument) 
  
  s <- corpus[[1]]$content
  
  # Split sentence into words 
  tokenizedSentence = strsplit(s, " ")[[1]]
  sentenceLength    = length(tokenizedSentence)
  
  # Look for ngrams in the model which complete the sentence using a backoff scheme
  fivegramCandidates  <- modelFivegrams[which(modelFivegrams$fisrtWord==tokenizedSentence[sentenceLength - 3] & modelFivegrams$secondWord==tokenizedSentence[sentenceLength - 2] & modelFivegrams$thirdWord==tokenizedSentence[sentenceLength - 1] & modelFivegrams$fourthWord==tokenizedSentence[sentenceLength]),]
  tetragramCandidates <- modelTetragrams[which(modelTetragrams$firstWord==tokenizedSentence[sentenceLength - 2] & modelTetragrams$secondWord==tokenizedSentence[sentenceLength - 1] & modelTetragrams$thirdWord==tokenizedSentence[sentenceLength]),]
  trigramCandidates   <- modelTrigrams[which(modelTrigrams$firstWord==tokenizedSentence[sentenceLength - 1] & modelTrigrams$secondWord==tokenizedSentence[sentenceLength] & modelTrigrams$thirdWord!="^"),]
  bigramCandidates    <- modelBigrams[which(modelBigrams$firstWord==tokenizedSentence[sentenceLength] & modelBigrams$secondWord!="^"),]
  
  # Sort the resulting candidate completions by their likelihood
  fivegramCandidates <- fivegramCandidates[order(-fivegramCandidates$cnt), ]
  tetragramCandidates <- tetragramCandidates[order(-tetragramCandidates$cnt), ]
  trigramCandidates   <- trigramCandidates[order(-trigramCandidates$cnt), ]
  bigramCandidates    <- bigramCandidates[order(-bigramCandidates$cnt), ]
  
  # Construct an output list where the first entry is the word most likely to complete the sentence
  # and the second entry is a data frame with the other likely words, to be displayed as a histogram
  if(nrow(fivegramCandidates) > 0 & sentenceLength > 3)
  {
    topWord <- paste(s, fivegramCandidates[which.max(fivegramCandidates$cnt), c("fifthWord")], sep = " ")
    outList[[1]] <- topWord
    outList[[2]] <- fivegramCandidates[, c("fifthWord","cnt")]
    outList[[2]] <- outList[[2]][1:10, ]
    
    #Normalize probability for the 10 most common completions 
    colnames(outList[[2]]) <- c("word", "cnt")
    outList[[2]]$cnt <- outList[[2]]$cnt / sum(as.numeric(outList[[2]]$cnt), na.rm = TRUE)
    
    return(outList)      
  }
  else if(nrow(tetragramCandidates) > 0 & sentenceLength > 2)
  {
    topWord <- paste(s, tetragramCandidates[which.max(tetragramCandidates$cnt), c("fourthWord")], sep = " ")
    outList[[1]] <- topWord
    outList[[2]] <- tetragramCandidates[, c("fourthWord","cnt")]
    outList[[2]] <- outList[[2]][1:10, ]
    
    #Normalize probability for the 10 most common completions 
    colnames(outList[[2]]) <- c("word", "cnt")
    outList[[2]]$cnt <- outList[[2]]$cnt / sum(as.numeric(outList[[2]]$cnt), na.rm = TRUE)

    return(outList)      
  }
  else if(nrow(trigramCandidates) > 0 & sentenceLength > 1)
  {
    topWord <- paste(s, trigramCandidates[which.max(trigramCandidates$cnt), c("thirdWord")], sep = " ")
    outList[[1]] <- topWord
    outList[[2]] <- trigramCandidates[, c("thirdWord","cnt")]
    
    #Normalize probability for the 10 most common completions 
    outList[[2]] <- outList[[2]][1:10, ]
    colnames(outList[[2]]) <- c("word", "cnt")
    outList[[2]]$cnt <- outList[[2]]$cnt / sum(as.numeric(outList[[2]]$cnt), na.rm = TRUE)

    return(outList)
  }
  else if(nrow(bigramCandidates) > 0)
  {
    topWord <- paste(s, bigramCandidates[which.max(bigramCandidates$cnt), c("secondWord")], sep = " ")
    outList[[1]] <- topWord
    outList[[2]] <- bigramCandidates[, c("secondWord","cnt")]
    
    #Normalize probability for the 10 most common completions 
    outList[[2]] <- outList[[2]][1:10, ]
    colnames(outList[[2]]) <- c("word", "cnt")
    outList[[2]]$cnt <- outList[[2]]$cnt / sum(as.numeric(outList[[2]]$cnt), na.rm = TRUE)

    return(outList)
    
  }
  else
  {
    word <- c("")
    cnt  <- c(0)
    
    outList <- list()
    outList[[1]] <- "NO PREDICTION POSSIBLE"
    outList[[2]] <- data.frame(word,cnt)

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