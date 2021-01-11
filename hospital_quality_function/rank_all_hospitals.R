
## Ranking hospitals in all states

rank_all_hospital <- function(outcome, num = "best"){
  ## read the data file
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = F)
  
  ## specify conditions 
  condition <- c("heart attack", "heart failure", "pneumonia")
  
  # checking if the input outcome parameter is a valid outcome or not
  if(!outcome %in% condition){
    stop("Invalid outcome")
  }else if (outcome == "heart attack"){col_num = 11} else if (outcome == "heart attack") {col_num = 17} else {col_num = 23}
  outcome_num <- suppressWarnings(as.numeric(data[, col_num]))
  
  hospital_rank <- data[order(outcome_num,data[,2]),]
  
  hospital_name <- if(num == "best"){hospital_rank[1,2]} else if (num == "worst"){hospital_rank[nrow(hospital_rank),2]} else {hospital_rank[num,2]}
  return(hospital_name)
}

rank_all_hospital("heart attack","best")
