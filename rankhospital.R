rankhospital <- function(state, outcome, num = "best")
{
  
##read outcome data:
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

##check that state and outcome data are valid
if (!any(state == file$State)){
  stop("invalid state") }
##print "invalid state" if the state input doesn't match any State listed in 
##the source file
else if ((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE) {
  stop("invalid outcome") }
##print "invalid outcome" if the outcome input doesn't match any condition
##listed in the listed vector
  
##specify column number based on outcome (i.e. medical condition) entered:
if (outcome == "heart attack") { 
  columnNo <- 11 
  }
else if (outcome == "heart failure") {
  columnNo <- 17 
  }
else {
  columnNo <- 23
  }

##subset the data frame to the specified state
state_subset <- subset(file, State == state)   
  
##coerce data to numeric values:
state_subset[ ,columnNo] <- as.numeric(state_subset[ ,columnNo])

##sort rows by 30-day rate (and alphabetically for hospitals, useful for
##identical rates) in specific state/outcome:
ranked_subset <- state_subset[order(state_subset[ ,columnNo], state_subset[ ,2]), ]

##remove rows with NAs:
ranked_subset <- ranked_subset[(!is.na(ranked_subset[ ,columnNo])), ]

##define num input as the hospital rank
if (num == "best") {
  rank <- 1 
  }
if (num == "worst") {
  ##find number of hospitals that fit state + outcome specified, choose last place
  rank <- nrow(ranked_subset) 
  }
else { 
  (rank <- num)
  }
  
return(ranked_subset[rank, 2])
##returns the hospital name in the row specified by 'num'

}
