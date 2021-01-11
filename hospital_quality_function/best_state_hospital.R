## Finding the best hospital in a state
best_state_hospital <- function(state,outcome){
  ## read the data file
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = F)
  
  ## subset by state of interest
  states <- subset(data, data$State == state)
  
  ## specify conditions 
  condition <- c("heart attack", "heart failure", "pneumonia")
  rank <- c("best","worst")
  
  # checking if the input state parameter is a valid state or not
  if(nrow(states) <= 0){stop("Invalid state")}
  
  # checking if the input outcome parameter is a valid outcome or not
  if(!outcome %in% condition){
    stop("Invalid outcome")
  }else if (outcome == "heart attack"){col_num = 11} else if (outcome == "heart attack") {col_num = 17} else {col_num = 23}
  outcome_num <- suppressWarnings(as.numeric(states[, col_num]))
  
  hospital_rank <- states[order(outcome_num,states[,2]),]
  
  ## if 2 hospitals have the same value, seperate then using sorting
  hospital_name <- hospital_rank[1,2]
  return(hospital_name)
}

best_state_hospital("GA", "heart attack")


