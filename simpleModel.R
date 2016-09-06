# ----------------------------------------------
# First exploration of the SwiftKey text corpus
# for predictive text applications
# ----------------------------------------------

# PREDICTION FUNCTION BASED ON MODEL
predictNextWord <- function(sentence, model)
{
  probDist <- model[sentence,]
  prediction <- paste(sentence, colnames(model)[which.max(probDist)])
  print(prediction)
}

# MAIN FUNCTION
simpleModel <- function(){
  # Load required libraries
  library(ggplot2)
  library(NLP)
  library(openNLP)
  library(tm)
  library(ngram)

  blogText    <- readLines("en_US.blogs.txt", 4000, skipNul = TRUE, encoding = 'UTF-8')
  newsText    <- readLines("en_US.news.txt", 4000, skipNul = TRUE, encoding = 'UTF-8')
  twitterText <- readLines("en_US.twitter.txt", 4000, skipNul = TRUE, encoding = 'UTF-8')

  text        <- c(blogText, newsText, twitterText)
  text        <- lapply(text, function(x) x <- String(x))
  
  print("FLAG 1")

  # Remove unnecessary objects
  rm(blogText)
  rm(newsText)
  rm(twitterText)
  
  print("FLAG 2")
  
  
  # Break down text into individual sentences, not lines
  tokenAnnotator <- Maxent_Sent_Token_Annotator()
  annotatedText <- lapply(text, function(x) annotate(x, tokenAnnotator))

  print("FLAG 3")
  
  
  sentences <- vector(length = 0)
  for(i in seq(length(text)))
  {
    sentences <- c(sentences, text[[i]][annotatedText[[i]]])
  }
  
  corpus <- Corpus(VectorSource(sentences))
  
  print("FLAG 4")
  
  
  rm(sentences)

  # Process corpus
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1) 
  corpus <- tm_map(corpus, content_transformer(tolower))

  print("FLAG 5")
  
  
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("it's", "it is", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("i'm", "i am", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("isn't", "is not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("can't", "cannot", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("won't", "will not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("don't", "do not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("didn't", "did not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("wouldn't", "would not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("couldn't", "could not", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("i've", "i have", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("we've", "we have", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("i'll", "i will", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("you'll", "you will", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("they'll", "they will", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("we'll", "we will", x)))
  
  print("FLAG 6")
  
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("[^[:alnum:] ]", "", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("^\\s+|\\s+$", "", x)))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument) 
  
  print("FLAG 7")
  
  
  # Get processed corpus as text
  corpusText <- as.character(lapply(corpus, function(x) as.character(x)))
  
  rm(corpus)
  
  # Remove sentences with less than three words
  corpusText <- corpusText[lapply(corpusText,function(x) length(strsplit(x, " ", fixed = TRUE)[[1L]]))>2]
  
  print("FLAG 8")
  
  
  # Construct n-grams
  unigramList <- ngram(corpusText, n=1)
  bigramList  <- ngram(corpusText, n=2)
  trigramList <- ngram(corpusText, n=3)
  
  print("FLAG 9")
  
  
  rm(corpusText)
  
  # Construct tables of ngrams sorted by frequency
  unigramFreq <- get.phrasetable(unigramList)
  bigramFreq  <- get.phrasetable(bigramList)
  trigramFreq <- get.phrasetable(trigramList)
  
  print("FLAG 10")
  
  
  unigramFreq$ngrams <- sapply(unigramFreq$ngrams, trimws)
  bigramFreq$ngrams <- sapply(bigramFreq$ngrams, trimws)
  trigramFreq$ngrams <- sapply(trigramFreq$ngrams, trimws)
  
  print("FLAG 11")
  
  
  # Get most common ngrams
  unigramFreq <- unigramFreq[1:1000,]
  bigramFreq  <- bigramFreq[1:500,]
  
  print("FLAG 12")
  
  
  # Create Markov transition matrix for bigrams -> trigrams
  model <- matrix(0, nrow = nrow(bigramFreq), ncol = nrow(unigramFreq))
  rownames(model) <- bigramFreq$ngrams
  colnames(model) <- unigramFreq$ngrams
  
  print("FLAG 13")
  
  
  for(i in seq(nrow(unigramFreq)))
  {
    for(j in seq(nrow(bigramFreq)))
    {
      # Create candidate trigram by concatenating a bigram and a unigram
      candidate <- paste(bigramFreq[j,]$ngrams, unigramFreq[i,]$ngrams)
      
      # Determine if the candidate trigram actually exists in the corpus
      # If yes, determine its frequency and increase the counts in the model matrix
      knownTrigram <- candidate %in% trigramFreq$ngrams
      if(knownTrigram)
      {
        model[j,i] <- model[j,i] + trigramFreq[trigramFreq$ngrams==candidate,]$freq
      }
    }
  }
  
  print("FLAG 14")
  
  
  # The matrix is very sparse, so remove null rows and columns
  model <- model[apply(model,1,sum) > 0,]
  model <- model[,apply(model,2,sum) > 0]
  
  print("FLAG 15")
  
  # Normalize model
  sums <- rowSums(model)
  model <- model / sums
  model[is.nan(model)] <- 0.0
  
  # Sort the rows alphabetically
  model <- model[order(rownames(model)), ]
  
  # Save model to file
  write.table(model, file="myModel.txt")
  
  #predictNextWord("case of", model)
}