##Finding the best hospital in the state

best <- funtion(state,outcome) {
  ##read outcome data:
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##check that state and outcome data are valid
  if !any(state == file$State){
    stop("invalid state") }
  ##print "invalid state" if the state input doesn't match any State listed in 
  ##the source file
  if ((outcome %in% c(heart attack, heart failure, pneumonia)) == FALSE)
    stop("invalid outcome") }
  ##print "invalid outcome" if the outcome input doesn't match any condition
  ##listed in the listed vector
  
  ##return hospital name with lowest 30-day death rate
  state_subset <- subset(file, State == state)
    ##subsetting the data frame to the specified state
    ##specifying column based on outcome AKA medical condition entered:
  if (outcome == "heart attack") { 
    colnum <- 11 }
  if (outcome == "heart failure") {
    colnum <- 17 }
  else {colnum <- 23 }
  }
  }
}