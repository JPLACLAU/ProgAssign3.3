rankall <- function(outcome, num = "best"){
  ## Read outcome data
   data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data <- data[c(2, 7, 11, 17, 23)]
        names(data)[1] <- "name"
        names(data)[2] <- "state"
        names(data)[3] <- "heart attack"
        names(data)[4] <- "heart failure"
        names(data)[5] <- "pneumonia"
        # I think this is a better way to start defining our dataset, 
        #by naming the columns with the imputs we desire, we only export
        #the data from the outcoms.csv an then we assign that data the names we want
  
  ## Check that state and outcome are valid
 outcomes = c("heart attack", "heart failure", "pneumonia")
        if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
        ## this way you have the checking less complicated
        
        ## Maybe it is just me but I dont see the step by step 
        ##of the coding in an easy and understandable way 
        ## with "easy and understandable" what i mean is the lack of the 
        ## double ## with explanations before or after coding, so i understand 
        ## what that sections is suposed to do, example:
        ##
        ##  coding coding coding
        ## "##explanation here"
        ##
        ## In the end this is just my point of view, and i am not the professor so 

   ## This is for taking  only rows with actual data data
        data <- data[data[outcome] != 'Not Available', ]
        
  ## For each state, find the hospital of the given rank
  
  if (outcome == "heart attack"){
    condition <- 11
  } else if(outcome == "heart failure"){
    condition <- 17
  } else if(outcome == "pneumonia"){
    condition <- 23
  }
  
  dataframe <- na.omit(outcome_data[,c(7, 2, condition)])
  names(dataframe) <- c("State", "hospital", "outcome")
  sorted <- dataframe[order(dataframe$State, dataframe$outcome, dataframe$hospital),]
  results <- data.frame()
  
  for (i in seq_along(valid_state)){
    state_subset <- subset(sorted, State == valid_state[i])
    
    if(num == "best"){
    num <- 1
    }else if(num == "worst"){
      num <- nrow(state_subset)
    }else{
      num <- as.numeric(num)
    }
  results <- rbind(results, state_subset["hospital", "State"])
  }
  
  return(results)
  
}
