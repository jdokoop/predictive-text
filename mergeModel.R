mergeModel <- function()
{
  library(dplyr)
  library(plyr)
  
  combineModels <- function(model0, model1)
  {
    # Do a full outer join of the two data frames
    model <- merge(model0, model1, by=c("firstWord", "secondWord", "thirdWord", "fourthWord", "fifthWord"), all = TRUE)
    
    # Replace NAs by 0
    model[is.na(model)] <- 0
    
    # Drop unnecessary columns produced in merging
    model$cnt <- model$cnt.x + model$cnt.y
    model <- subset(model, select = -c(X.x, X.y, cnt.x, cnt.y) )
    
    # Select only ngrams with frequency > 1
    model <- model[which(model$cnt > 1),]
    
    return(model)    
  }

  numFiles <- 0  
  for(i in c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39))
  {
    model0 <- read.csv(sprintf("ModelFragments5g/myModel.%i.csv",i-1))
    print(sprintf("Read %i", i-1))
    model1 <- read.csv(sprintf("ModelFragments5g/myModel.%i.csv",i))
    print(sprintf("Read %i", i))
  
    model <- combineModels(model0, model1)
    rm(model0)
    rm(model1)
    print("Combined models...")
  
    write.csv(model, file=sprintf("MergedModels5g/myModelStepA.%i.csv", numFiles))
    numFiles <- numFiles + 1
  }
  
  numFiles <- 0
  for(i in c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19))
  {
    model0 <- read.csv(sprintf("MergedModels5g/myModelStepA.%i.csv",i-1))
    print(sprintf("Read %i", i-1))
    model1 <- read.csv(sprintf("MergedModels5g/myModelStepA.%i.csv",i))
    print(sprintf("Read %i", i))
    
    model <- combineModels(model0, model1)
    rm(model0)
    rm(model1)
    print("Combined models...")
    
    write.csv(model, file=sprintf("MergedModels5g/myModelStepB.%i.csv", numFiles))
    numFiles <- numFiles + 1
  }
  
  numFiles <- 0
  for(i in c(1, 3, 5, 7, 9))
  {
    model0 <- read.csv(sprintf("MergedModels5g/myModelStepB.%i.csv",i-1))
    print(sprintf("Read %i", i-1))
    model1 <- read.csv(sprintf("MergedModels5g/myModelStepB.%i.csv",i))
    print(sprintf("Read %i", i))
    
    model <- combineModels(model0, model1)
    rm(model0)
    rm(model1)
    print("Combined models...")
    
    write.csv(model, file=sprintf("MergedModels5g/myModelStepC.%i.csv", numFiles))
    numFiles <- numFiles + 1
  }
  
  numFiles <- 0

  model0 <- read.csv(sprintf("MergedModels5g/myModelStepC.%i.csv",0))
  print(sprintf("Read %i", 0))
  model1 <- read.csv(sprintf("MergedModels5g/myModelStepC.%i.csv",1))
  print(sprintf("Read %i", 1))
    
  model <- combineModels(model0, model1)
  rm(model0)
  rm(model1)
  print("Combined models...")
    
  write.csv(model, file=sprintf("MergedModels5g/myModelStepD.%i.csv", 0))
  
  numFiles <- 0
  
  model0 <- read.csv(sprintf("MergedModels5g/myModelStepC.%i.csv",2))
  print(sprintf("Read %i", 2))
  model1 <- read.csv(sprintf("MergedModels5g/myModelStepC.%i.csv",3))
  print(sprintf("Read %i", 3))
  
  model <- combineModels(model0, model1)
  rm(model0)
  rm(model1)
  print("Combined models...")
  
  write.csv(model, file=sprintf("MergedModels5g/myModelStepD.%i.csv", 1))
  
  numFiles <- 0
  
  model0 <- read.csv(sprintf("MergedModels5g/myModelStepD.%i.csv",0))
  print(sprintf("Read %i", 0))
  model1 <- read.csv(sprintf("MergedModels5g/myModelStepD.%i.csv",1))
  print(sprintf("Read %i", 1))
  
  model <- combineModels(model0, model1)
  rm(model0)
  rm(model1)
  print("Combined models...")
  
  write.csv(model, file=sprintf("MergedModels5g/myModelStepFinal.%i.csv", 0))
}