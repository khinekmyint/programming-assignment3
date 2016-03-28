rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that outcome are valid  
  `%notin%` <- Negate(`%in%`)
  conditions <- c("heart attack","heart failure","pneumonia")
  if (outcome %notin% conditions) {
    stop('invalid outcome')
  }
  
  ## name the column of the rate
  condCol <- if(outcome == 'heart attack')
  {condCol <- 11}
  else if
  (outcome == 'heart failure')
  {condCol <- 17}
  else if
  (outcome == 'pneumonia')
  {condCol <- 23}
  
  ##set the unique state and sort it
  theStates <- unique(data[,7])
  theStates<-sort(theStates)
  n<-length(theStates)
  bestinstate<-NULL
  worst<-"worst"
  
  ##find the desire hospital in each state
  for(i in 1:n){
    tstate<-theStates[i]
    subData <- data[data$State == tstate,c(2,7,condCol)]
    names1 <- c('Hospital','State','Rate')
    names(subData) <- names1
    subData$Rate <- as.numeric(as.character(subData$Rate))
    subData2 <- complete.cases(subData)
    subData3 <- subData[subData2,]
    subDataSort <- subData3[order(subData3$Rate,subData3$Hospital),]
    
    #check the num
    if(num=="best"){
      num<-1
    }
    else if(num ==worst){
      worst<-nrow(subData3)
      num<-worst
    }
    
    else if(as.numeric(num)) {
      num<-as.numeric(num)
    }
    else{
      stop("invalid num")
    }
    
    rateinstate<-rbind(rateinstate,subDataSort[num,])
    print(num)
    k<-nrow(subDataSort)
    if(k<num){
      rateinstate[i,2]=tstate
    }
  }
  rateinstate
}
