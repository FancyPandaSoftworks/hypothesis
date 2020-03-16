rm(list=ls())

library(hier.part)


###True labeling function F, not used for now
correctF <- list(1, 0, 1, 1)

###Input: a hypothesis that is a list 
###Check whether it is a positive hypothesis by comparing the two lists 
###Return true if +1, return false if -1
posExample <- function(example){
  correctFunction <-  list(list(2,1), list(4,0), list(6,1), list(8,1))
  for (i in 1:length(correctFunction)) {
    if(example[correctFunction[[i]][[1]]] != correctFunction[[i]][[2]]){
      return(FALSE)
    }
    else{
      next
    }
  }
  return(TRUE)
}


example <- list(1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1)
posExample(example)

###Input: hypothesis HD
###Calculate the loss of the hypothesis 
###Output: loss 
lossFunction <- function(hypothesis){
  
  ###total number of 0 and 1 
  countValue <- sum(do.call(rbind, lapply(hypothesis, lengths)))
  
  ###The probability of true F
  probF <- 0.5^4
  
  ###probability of H 
  probH <- 0.5^countValue
  
  ###Calculate true loss
  trueLoss <- probF - probH
  return(trueLoss)
} 




###Input: examples
###Create a hypothesis out of the examples, remove 1 if 0 occurs, and remove 0 if 1 occurs given the position
###Output: A hypothesis 
createHypothesis <- function(size){
  hypothesis <- rep(list(list(0,1)), 20)
  
  positiveExample <- sampleSelection(size)
  for (i in 1:length(positiveExample)) {
    for (j in 1:length(positiveExample[[i]])) {
      if(positiveExample[[i]][[j]] == 1){
        if(is.na(match(0, hypothesis[[j]]))){
          next
        }
        else{
          hypothesis[[j]][[match(0, hypothesis[[j]])]] <- NULL
        }
      }
      if(positiveExample[[i]][[j]] == 0){
        if(is.na(match(1, hypothesis[[j]]))){
          next
        }
        else{
          hypothesis[[j]][[match(1, hypothesis[[j]])]] <- NULL
        }
      }
    }
  }
  hypothesis <- unlist(hypothesis, recursive = FALSE)
  return(hypothesis)
}

###Input: The sample size that we need to draw
###After drawing a sample, we need to make sure that there is at least one sample positive
###Output: Samples that contain positive samples
###Example: sampleSelection(50) -> list(list(0,1,0,1,1,1,1,1), list(1,1,0,1,0,0,1,1))
sampleSelection <- function(size){
  
  ###list of samples
  totalSample <- list()
  ###Sample the possibilities
  for (i in 1:size) {
    totalSample <- append(totalSample, list(exampleGenerator(20)))
  }
  
  
  ###list of positive examples
  positiveExample <- list()
  
  ###Check whether at least one is positive
  for (i in 1:length(totalSample)) {
    if(posExample(totalSample[[i]])){
      positiveExample <- append(positiveExample, list(totalSample[[i]]))
    }
    else{
      next
    }
  }
  if(length(positiveExample)>0){
    return(positiveExample)
  }
  else{
    return(sampleSelection(size))
  }
    
}

###Input: None
###Create a random example with 0 and 1
###Output: An example of a certain length(In our case, the parameter should be 20)
###Used in the function sampleSelection
###Example: exampleGenerator(5) -> list(0,1,0,1,1)
exampleGenerator <- function(listLength){
  example <- list()
  
  for (i in 1:listLength) {
    value <- sample(c(0, 1), 1)
    example <- append(example, value)
  }
  return(example)
}



compareHypothesis <- function(hypothesis){
  if(identical(hypothesis, correctF)){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


experiment <- function(size){
  
  lossEstimation <- c()
  positiveError <- 0
  positiveEqual <- 0
  positiveErrorEstimation <- 0
  for (i in 1:10000) {
    
    ###Create hypothesis
    hypothesis <- createHypothesis(size)
    isEqual <- compareHypothesis(hypothesis = hypothesis)
    
    if(isEqual){
      positiveEqual <- positiveEqual + 1
    }
    
    trueLoss <- lossFunction(hypothesis)
    lossEstimation <- append(lossEstimation, trueLoss)
    if(trueLoss <= 0.05){
      positiveError <- positiveError + 1
    }
  }
  table <- data.frame(sample = size, errorBelowThreshold = positiveError/10000, identicalH = positiveEqual/10000, average = mean(lossEstimation))
  return(table)
}
experiment(50)



totalExperiment <- function(){
  for (i in seq(50, 500, 50)) {
    result <- experiment(i)
    print(result)
  }
  
}

totalExperiment()
