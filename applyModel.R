applyModel <- function(sentence)
{
  # Read model from file
  model <- read.csv("myModelStepFinal.0.csv")
  
  # PREDICTION FUNCTION BASED ON MODEL
  predictNextWord <- function(s)
  {
    # TODO: Apply same transformations to sentence as to training corpus
    
    # Split sentence into words 
    tokenizedSentence = strsplit(s, " ")[[1]]
  
    # Separate the n-grams into different tables
    modelBigrams    <- model[which(model$thirdWord=='^' & model$fourthWord=='^'),]
    modelTrigrams   <- model[which(model$thirdWord!='^' & model$fourthWord=='^'),]
    modelTetragrams <- model[which(model$fourthWord!='^'),]

    # Check if there is a known tetragram starting with the last three words of the sentence
    tetragramCandidates <- modelTetragrams[which(modelTetragrams$firstWord==tokenizedSentence[1] & modelTetragrams$secondWord==tokenizedSentence[2] & modelTetragrams$thirdWord==tokenizedSentence[3]),]
    trigramCandidates   <- modelTrigrams[which(modelTrigrams$firstWord==tokenizedSentence[2] & modelTrigrams$secondWord==tokenizedSentence[3] & modelTrigrams$thirdWord!="^"),]
    bigramCandidates    <- modelBigrams[which(modelBigrams$firstWord==tokenizedSentence[3] & modelBigrams$secondWord!="^"),]
    
    if(nrow(tetragramCandidates) > 0)
    {
      print("Using tetragrams")
      result <- paste(sentence, tetragramCandidates[which.max(tetragramCandidates$cnt), "fourthWord"], sep = " ")
      print(result)      
    }
    else if(nrow(trigramCandidates) > 0)
    {
      print("Using trigrams")
      result <- paste(sentence, trigramCandidates[which.max(trigramCandidates$cnt), "thirdWord"], sep = " ")
      print(result)
    }
    else if(nrow(bigramCandidates) > 0)
    {
      print("Using bigrams")
      result <- paste(sentence, bigramCandidates[which.max(bigramCandidates$cnt), "secondWord"], sep = " ")
      print(result)
    }
    else
    {
      print("NO COMPLETION FOUND")
    }
  }
  
  predictNextWord(sentence)
}