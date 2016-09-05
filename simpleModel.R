# ----------------------------------------------
# First exploration of the SwiftKey text corpus
# for predictive text applications
# ----------------------------------------------

# MAIN FUNCTION
simpleModel <- function(){
  # Load required libraries
  library(ggplot2)
  library(NLP)
  library(openNLP)
  library(tm)
  library(ngram)

  blogText    <- readLines("en_US.blogs.txt", 100, skipNul = TRUE, encoding = 'UTF-8')
  newsText    <- readLines("en_US.news.txt", 100, skipNul = TRUE, encoding = 'UTF-8')
  twitterText <- readLines("en_US.twitter.txt", 100, skipNul = TRUE, encoding = 'UTF-8')

  text        <- c(blogText, newsText, twitterText)
  text        <- lapply(text, function(x) x <- String(x))

  # Remove unnecessary objects
  rm(blogText)
  rm(newsText)
  rm(twitterText)
  
  # Break down text into individual sentences, not lines
  tokenAnnotator <- Maxent_Sent_Token_Annotator()
  annotatedText <- lapply(text, function(x) annotate(x, tokenAnnotator))

  sentences <- vector(length = 0)
  for(i in seq(length(text)))
  {
    sentences <- c(sentences, text[[i]][annotatedText[[i]]])
  }
  
  corpus <- Corpus(VectorSource(sentences))
  
  rm(sentences)

  # Process corpus
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1) 
  corpus <- tm_map(corpus, content_transformer(tolower))

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
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("[^[:alnum:] ]", "", x)))
  corpus <- tm_map(corpus, content_transformer( function(x) gsub("^\\s+|\\s+$", "", x)))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, PlainTextDocument) 
  
  # Get processed corpus as text
  corpusText <- as.character(lapply(corpus, function(x) as.character(x)))
  
  rm(corpus)
  
  # Remove sentences with less than three words
  corpusText <- corpusText[lapply(corpusText,function(x) length(strsplit(x, " ", fixed = TRUE)[[1L]]))>2]
  
  rm(corpusText)
  
  # Construct n-grams
  unigramList <- ngram(corpusText, n=1)
  bigramList  <- ngram(corpusText, n=2)
  trigramList <- ngram(corpusText, n=3)
  
  # Construct tables of ngrams sorted by frequency
  unigramFreq <- get.phrasetable(unigramList)
  bigramFreq  <- get.phrasetable(bigramList)
  trigramFreq <- get.phrasetable(trigramList)
  
  unigramFreq$ngrams <- sapply(unigramFreq$ngrams, trimws)
  bigramFreq$ngrams <- sapply(bigramFreq$ngrams, trimws)
  trigramFreq$ngrams <- sapply(trigramFreq$ngrams, trimws)
  
  # Get most common ngrams
  unigramFreq <- unigramFreq[1:300,]
  bigramFreq  <- bigramFreq[1:150,]
  
  # Create Markov transition matrix for bigrams -> trigrams
  model <- matrix(0, nrow = nrow(bigramFreq), ncol = nrow(unigramFreq))
  rownames(model) <- bigramFreq$ngrams
  colnames(model) <- unigramFreq$ngrams
  
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
  
  # The matrix is very sparse, so remove null rows and columns
  model <- model[apply(model,1,sum) > 0,]
  model <- model[,apply(model,2,sum) > 0]
  
  # Sort the rows alphabetically
  model <- model[order(rownames(model)), ]
  
  model

}