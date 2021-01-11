## Ranking hospitals by outcome in a state
rank_state_hospital <- function(state,outcome, num = "best"){
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
  
  hospital_name <- if(num == "best"){hospital_rank[1,2]} else if (num == "worst"){hospital_rank[nrow(hospital_rank),2]} else {hospital_rank[num,2]}
  return(hospital_name)
}

rank_state_hospital("GA", "heart attack","best")
rank_state_hospital("GA", "heart attack","worst")
