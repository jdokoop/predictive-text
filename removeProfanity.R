#-------------------------------------------------------------------------------------------------
# Script to remove profanity from the text prediction model
# A list of profane words was obtained from https://www.cs.cmu.edu/~biglou/resources/bad-words.txt
# Notice that the above list does not contain just profanity, but also common words which are
# sometimed used in offensive contexts
#-------------------------------------------------------------------------------------------------

removeProfanity <- function()
{
  # Read in list of sensistive words from file
  dictionary <- read.table("sensitiveWordList.txt")
  colnames(dictionary) <- c("profanity")
  
  # Read in model data frame from file
  model <- read.csv("myModelStepFinal.0.csv")
  model$index <- seq(1:nrow(model))
  nrowBefore <- nrow(model)
  
  # Do a right outer join of the two data frames to identify profanity in the first word of the ngram
  merge1 <- merge(x = model, y = dictionary, by.x = "firstWord", by.y = "profanity", all.y = TRUE)
  merge1 <- na.omit(merge1)
  model <- model[-merge1$index, ]
  
  # Do a right outer join of the two data frames to identify profanity in the second word of the ngram
  model$index <- seq(1:nrow(model))
  merge2 <- merge(x = model, y = dictionary, by.x = "secondWord", by.y = "profanity", all.y = TRUE)
  merge2 <- na.omit(merge2)
  model <- model[-merge2$index, ]
  
  # Do a right outer join of the two data frames to identify profanity in the third word of the ngram
  model$index <- seq(1:nrow(model))
  merge3 <- merge(x = model, y = dictionary, by.x = "thirdWord", by.y = "profanity", all.y = TRUE)
  merge3 <- na.omit(merge3)
  model <- model[-merge3$index, ]
  
  # Do a right outer join of the two data frames to identify profanity in the fourth word of the ngram
  model$index <- seq(1:nrow(model))
  merge4 <- merge(x = model, y = dictionary, by.x = "fourthWord", by.y = "profanity", all.y = TRUE)
  merge4 <- na.omit(merge4)
  model <- model[-merge4$index, ]
  
  # Calculate fraction of model that is removed due to profanity
  fractionLost <- nrow(model) / nrowBefore
  print(paste("Removed", fractionLost, "% of the model"))
  
  # Save the model to file
  write.csv(model, file="myModelProfanityRemoved.csv")
}