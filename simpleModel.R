# ----------------------------------------------
# First exploration of the SwiftKey text corpus
# for predictive text applications
# ----------------------------------------------

# MAIN FUNCTION
treeModel <- function(){
  # Load required libraries
  library(ggplot2)
  library(NLP)
  library(openNLP)
  library(tm)
  library(ngram)
  library(data.tree)
  
  processFile <- function(fileNum){
    blogText    <- readLines(sprintf("FileFragments/blogs.%i.txt", fileNum), 10000, skipNul = TRUE, encoding = 'UTF-8')
    newsText    <- readLines(sprintf("FileFragments/news.%i.txt", fileNum), 10000, skipNul = TRUE, encoding = 'UTF-8')
    twitterText <- readLines(sprintf("FileFragments/twitter.%i.txt", fileNum), 10000, skipNul = TRUE, encoding = 'UTF-8')
    
    text        <- c(blogText, newsText, twitterText)
    text        <- lapply(text, function(x) x <- String(x))
    
    print(sprintf("Processing file chunk %i", fileNum))
    print("Read data")
    
    # Remove unnecessary objects
    rm(blogText)
    rm(newsText)
    rm(twitterText)
    
    # Break down text into individual sentences, not lines
    tokenAnnotator <- Maxent_Sent_Token_Annotator()
    annotatedText <- lapply(text, function(x) annotate(x, tokenAnnotator))
    
    print("Created sentences")
    
    sentences <- vector(length = 0)
    for(i in seq(length(text)))
    {
      sentences <- c(sentences, text[[i]][annotatedText[[i]]])
    }
    
    corpus <- Corpus(VectorSource(sentences))
    
    print("Created corpus")
    
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
    
    print("Finished corpus transformation")
    
    # Get processed corpus as text
    corpusText <- as.character(lapply(corpus, function(x) as.character(x)))
    
    rm(corpus)
    
    # Remove sentences with less than four words
    corpusText <- corpusText[lapply(corpusText,function(x) length(strsplit(x, " ", fixed = TRUE)[[1L]]))>3]
    
    # Construct n-grams
    unigramList   <- ngram(corpusText, n=1)
    bigramList    <- ngram(corpusText, n=2)
    trigramList   <- ngram(corpusText, n=3)
    tetragramList <- ngram(corpusText, n=4)
    
    print("Finished constructing ngrams")
    
    rm(corpusText)
    
    # Construct tables of ngrams sorted by frequency
    bigramFreq    <- get.phrasetable(bigramList)
    trigramFreq   <- get.phrasetable(trigramList)
    tetragramFreq <- get.phrasetable(tetragramList)
    
    # Get only ngrams with a frequency greater than some threshold
    #bigramFreq <- bigramFreq[which(bigramFreq$freq > 2),]
    #trigramFreq <- trigramFreq[which(trigramFreq$freq > 2),]
    #tetragramFreq <- tetragramFreq[which(tetragramFreq$freq > 2),]
    
    print("Finished constructing phrasetables")
    
    bigramFreq$ngrams    <- sapply(bigramFreq$ngrams, trimws)
    trigramFreq$ngrams   <- sapply(trigramFreq$ngrams, trimws)
    tetragramFreq$ngrams <- sapply(tetragramFreq$ngrams, trimws)
    
    # Get number of ngrams to include in the final model
    #numBigrams <- ceiling(nrow(bigramFreq) * 0.5)
    #numTrigrams <- ceiling(nrow(trigramFreq) * 0.6)
    #numTetragrams <- ceiling(nrow(tetragramFreq) * 0.7)
    
    # Get most common ngrams
    #bigramFreq  <- bigramFreq[1:numBigrams,]
    #trigramFreq <- trigramFreq[1:numTrigrams,]
    #tetragramFreq <- tetragramFreq[1:numTetragrams,]
    
    firstWordBigrams   <- as.character(sapply(bigramFreq$ngrams, function(x) strsplit(x, " ")[[1]][1]))
    secondWordBigrams  <- as.character(sapply(bigramFreq$ngrams, function(x) strsplit(x, " ")[[1]][2]))
    thirdWordBigrams   <- rep("^",nrow(bigramFreq))
    fourthWordBigrams  <- rep("^",nrow(bigramFreq))
    
    firstWordTrigrams   <- as.character(sapply(trigramFreq$ngrams, function(x) strsplit(x, " ")[[1]][1]))
    secondWordTrigrams  <- as.character(sapply(trigramFreq$ngrams, function(x) strsplit(x, " ")[[1]][2]))
    thirdWordTrigrams   <- as.character(sapply(trigramFreq$ngrams, function(x) strsplit(x, " ")[[1]][3]))
    fourthWordTrigrams  <- rep("^",nrow(trigramFreq))
    
    firstWordTetragrams   <- as.character(sapply(tetragramFreq$ngrams, function(x) strsplit(x, " ")[[1]][1]))
    secondWordTetragrams  <- as.character(sapply(tetragramFreq$ngrams, function(x) strsplit(x, " ")[[1]][2]))
    thirdWordTetragrams   <- as.character(sapply(tetragramFreq$ngrams, function(x) strsplit(x, " ")[[1]][3]))
    fourthWordTetragrams  <- as.character(sapply(tetragramFreq$ngrams, function(x) strsplit(x, " ")[[1]][4]))
    
    firstWord  <- c(firstWordBigrams, firstWordTrigrams, firstWordTetragrams)
    secondWord <- c(secondWordBigrams, secondWordTrigrams, secondWordTetragrams)
    thirdWord  <- c(thirdWordBigrams, thirdWordTrigrams, thirdWordTetragrams)
    fourthWord <- c(fourthWordBigrams, fourthWordTrigrams, fourthWordTetragrams)
    cnt        <- c(bigramFreq$freq, trigramFreq$freq, tetragramFreq$freq)
    
    # Construct tree from ngrams
    splitNgrams <- data.frame(firstWord, secondWord, thirdWord, fourthWord, cnt)
    write.csv(splitNgrams, file=sprintf("ModelFragments/myModel.%i.csv", fileNum)) 
  }
  
  for(i in seq(1:40))
  {
    processFile(i-1)
  }
}