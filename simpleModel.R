# ----------------------------------------------
# First exploration of the SwiftKey text corpus
# for predictive text applications
# ----------------------------------------------

# FUNCTION TO TOKENIZE BIGRAMS
BigramTokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min = 2, max = 2))  
}

# FUNCTION TO TOKENIZE TRIGRAMS
TrigramTokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min = 3, max = 3))  
}

# MAIN FUNCTION
simpleModel <- function(){
  # Load required libraries
  library(tm)
  library(RWeka)
  library(ggplot2)
  library(combinat)
  library(gtools)
  library(slam)
  library(SnowballC)
  
  # Create corpus from blog, news, and twitter sample in plain text file
  blogText    <- readLines("en_US.blogs.txt", 200, skipNul = TRUE, encoding = 'UTF-8')
  newsText    <- readLines("en_US.news.txt", 200, skipNul = TRUE, encoding = 'UTF-8')
  twitterText <- readLines("en_US.twitter.txt", 200, skipNul = TRUE, encoding = 'UTF-8')
  text        <- paste(blogText, newsText, twitterText)
  corpus      <- Corpus(VectorSource(text))
  
  # Process corpus
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1) 
  corpus <- tm_map(corpus, content_transformer(tolower), mc.cores=1)
  
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
  
  corpus <- tm_map(corpus, removePunctuation, mc.cores=1)
  corpus <- tm_map(corpus, removeNumbers, mc.cores=1)
  corpus <- tm_map(corpus, stripWhitespace, mc.cores=1)
  corpus <- tm_map(corpus, PlainTextDocument)   
  
  # Create document term matrices for 1-, 2-, and 3-grams
  dtmUnigram <- DocumentTermMatrix(corpus)
  dtmBigram  <- DocumentTermMatrix(corpus, control=list(tokenize = BigramTokenizer))
  dtmTrigram <- DocumentTermMatrix(corpus, control=list(tokenize = TrigramTokenizer))
 
  # Create data frame indicating frequency of unigrams
  unigramFreq <- sort(colSums(as.matrix(dtmUnigram)), decreasing = TRUE)
  unigramFreq <- data.frame(word = names(unigramFreq), frequency = unigramFreq)
  
  # Create data frame indicating frequency of bigrams
  bigramFreq <- sort(colSums(as.matrix(dtmBigram)), decreasing = TRUE)
  bigramFreq <- data.frame(word = names(bigramFreq), frequency = bigramFreq)
  
  # Create data frame indicating frequency of trigrams
  trigramFreq <- sort(colSums(as.matrix(dtmTrigram)), decreasing = TRUE)
  trigramFreq <- data.frame(word = names(trigramFreq), frequency = trigramFreq)
  
  # Create dictionary from the most common unigrams
  dictionary <- as.character(unigramFreq[1:10,]$word)
  
  # Select the most common trigrams
  commonBigrams <- bigramFreq[1:10,]
  
  # Create a second-order Markov chain transition matrix
  # Each matrix entry corresponds to how many times the trigram 'model[i,j]' occurs in the corpus
  model <- matrix(0, nrow = nrow(commonBigrams), ncol = length(dictionary))
  rownames(model) <- commonBigrams$word
  colnames(model) <- dictionary

  for(i in seq(length(dictionary)))
  {
    for(j in seq(nrow(commonBigrams)))
    {
      # Create candidate trigram by concatenating a bigram and a unigram
      candidate <- paste(commonBigrams[j,]$word, dictionary[i])
      
      # Determine if the candidate trigram actually exists in the corpus
      # If yes, determine its frequency and increase the counts in the model matrix
      knownTrigram <- candidate %in% trigramFreq$word
      if(knownTrigram)
      {
        model[j,i] <- model[j,i] + trigramFreq[candidate,]$frequency
      }
    }
  }

  # Normalize model so probabilities add up to 1 in every row
  sums <- rowSums(model)
  model <- model / sums
  model[is.nan(model)] <- 0.0
  
  model
}