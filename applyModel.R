applyModel <- function(sentence)
{
  # Read model from file
  model <- read.csv("myModelStepFinal.0.csv")
  
  # Separate the n-grams into different tables
  modelBigrams    <- model[which(model$thirdWord=='^' & model$fourthWord=='^'),]
  modelTrigrams   <- model[which(model$thirdWord!='^' & model$fourthWord=='^'),]
  modelTetragrams <- model[which(model$fourthWord!='^'),]
  
  # PREDICTION FUNCTION BASED ON MODEL
  predictNextWord <- function(s)
  {
    # TODO: Apply same transformations to sentence as to training corpus
    
    # Split sentence into words 
    tokenizedSentence = strsplit(s, " ")[[1]]
    sentenceLength    = length(tokenizedSentence)
    
    # Look for ngrams in the model which complete the sentence using a backoff scheme
    tetragramCandidates <- modelTetragrams[which(modelTetragrams$firstWord==tokenizedSentence[sentenceLength - 2] & modelTetragrams$secondWord==tokenizedSentence[sentenceLength - 1] & modelTetragrams$thirdWord==tokenizedSentence[sentenceLength]),]
    trigramCandidates   <- modelTrigrams[which(modelTrigrams$firstWord==tokenizedSentence[sentenceLength - 1] & modelTrigrams$secondWord==tokenizedSentence[sentenceLength] & modelTrigrams$thirdWord!="^"),]
    bigramCandidates    <- modelBigrams[which(modelBigrams$firstWord==tokenizedSentence[sentenceLength] & modelBigrams$secondWord!="^"),]
    
    # Sort the resulting candidate completions by their likelihood
    tetragramCandidates <- tetragramCandidates[order(-tetragramCandidates$cnt), ]
    trigramCandidates   <- trigramCandidates[order(-trigramCandidates$cnt), ]
    bigramCandidates    <- bigramCandidates[order(-bigramCandidates$cnt), ]
    
    # Use backoff scheme depending on sentence length
    # This way we have a prediction regardless of how many words the user types
    if(nrow(tetragramCandidates) > 0 & sentenceLength > 2)
    {
      topWord <- paste(s, tetragramCandidates[which.max(tetragramCandidates$cnt), c("fourthWord")], sep = " ")
      return(topWord)      
    }
    else if(nrow(trigramCandidates) > 0 & sentenceLength > 1)
    {
      topWord <- paste(s, trigramCandidates[which.max(trigramCandidates$cnt), c("thirdWord")], sep = " ")
      return(topWord)      
    }
    else if(nrow(bigramCandidates) > 0)
    {
      topWord <- paste(s, bigramCandidates[which.max(bigramCandidates$cnt), c("secondWord")], sep = " ")
      return(topWord)      
    }
    else
    {
      return("---")
    }
  }
  
  prediction <- predictNextWord(sentence)
  print(prediction)
}