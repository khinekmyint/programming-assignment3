best <- function(state, outcome) {
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")

  ## Check that state and outcome are valid  
  `%notin%` <- Negate(`%in%`)
  theStates <- unique(data[,7])
  if (state %notin% theStates){
    stop('invalid state')
  }
  
  conditions <- c("heart attack","heart failure","pneumonia")
  if (outcome %notin% conditions) {
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with lowest 30-day death rates
  condCol <- if(outcome == 'heart attack')
  {condCol <- 11}
  else if
  (outcome == 'heart failure')
  {condCol <- 17}
  else if
  (outcome == 'pneumonia')
  {condCol <- 23}
  
  subData <- data[data$State == state,c(2,7,condCol)]
  
  names1 <- c('Hospital','State','Rate')
  names(subData) <- names1
  subData$Rate <- as.numeric(as.character(subData$Rate))
  subData2 <- complete.cases(subData)
  subData3 <- subData[subData2,]
  subDataSort <- subData3[order(subData3$Rate,subData3$Hospital),]
  subDataSort[1,c(1,3)]
}
