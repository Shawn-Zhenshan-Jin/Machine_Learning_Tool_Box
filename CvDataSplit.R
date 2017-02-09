#########################################################
##
##  N-folds based cross validation data split mechanism
##  Parameters:
##  cvIdx: From 1 to combn(totalFold, TrainingFold).index of one specific data split in cross validatoin 
##  testPer: percentage of test dataset, e.g. testPer = 20
##
##  Return:
##  List of training data x,y and validation data x,y
##
#########################################################
CvDataSplit = function(input, output, totalFold, trainFoldNum, cvIdx, seed, test = FALSE, testPer = NULL) {
  input <- as.data.frame(input); output <- as.data.frame(output)
  set.seed(seed)
  
  # Get training folds index
  foldComb = combn(totalFold, trainFoldNum)[, sample(ncol(combn(totalFold, trainFoldNum)))]
  trainFoldIdx = foldComb[,cvIdx]
  # Shuffle the observation index of input 
  indexShuffled = sample(1:nrow(input),nrow(input), replace = FALSE)
  input = input[indexShuffled,]
  output = as.data.frame(output[indexShuffled,])
  
  if(test){
    testNum <- trunc(nrow(input) * (testPer/100),0)
    testX <- input[1:testNum,]
    input <- input[(testNum + 1):nrow(input), ]
    testY <- output[1:testNum,]
    output <- output[(testNum + 1):nrow(output), ]
  }
  
  # Generate the training slicing indicator vector
  foldIdx <- rep(1:totalFold, each = nrow(input)/totalFold)
  foldIdx <- c(foldIdx, rep(totalFold, times = nrow(input)%%totalFold))
  #foldIdx <- append(foldIdx, rep(totalFold, nrow(input) - length(foldIdx)))
  TrainIndicator <- sapply(foldIdx, function(foldIdxx) if(foldIdxx %in% trainFoldIdx) return(TRUE)
                           else return(FALSE))
  
  input <- as.data.frame(input); output <- as.data.frame(output)
  
  if(test){
    return(list(trainX = input[TrainIndicator,], trainY = output[TrainIndicator,],
                validX = input[!TrainIndicator,], validY = output[!TrainIndicator,],
                testX = testX, testY = testY))
  }else{
    return(list(trainX = input[TrainIndicator,], trainY = output[TrainIndicator,],
                validX = input[!TrainIndicator,], validY = output[!TrainIndicator,]))
  }
}
