rankhospital <- function(state, outcome, num = "best") {
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
  
  condCol <- if(outcome == 'heart attack')
  {condCol <- 11}
  else if
  (outcome == 'heart failure')
  {condCol <- 17}
  else if
  (outcome == 'pneumonia')
  {condCol <- 23}
  
  ## Return the desire hospital name in that state 
  subData <- data[data$State == state,c(2,7,condCol)]
  names1 <- c('Hospital','State','Rate')
  names(subData) <- names1
  subData$Rate <- as.numeric(as.character(subData$Rate))
  subData2 <- complete.cases(subData)
  subData3 <- subData[subData2,]
  subDataSort <- subData3[order(subData3$Rate,subData3$Hospital),]
  subDataSort[1,c(1,3)]
  
  if(num=="best"){
    f<-1
  }
  else if(num=="worst"){
    f<-nrow(subDataSort)
  }
    
  else if(as.numeric(num)) {
    f<-as.numeric(num)
  }
  else{
    stop("invalid num")
  }
  
  subDataSort[f,c(1,3)]
}
