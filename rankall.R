rankall <- function(outcome, num = "best") {
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  if ((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE) {
    stop("invalid outcome") }

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
  
  ##coerce outcome data to numeric values:
  file[ ,columnNo] <- as.numeric(file[ ,columnNo])
  
  ##remove rows with NAs:
  file <- file[(!is.na(file[ ,columnNo])), ]
  
  ##split the data frame by state
  state_split <- split(file, file$State)
  
  ans <- lapply(state_split, function(x, num) {
    ##create a function ordering the split state info by outcome and hosp name
    x = x[order(x[, columnNo], x$Hospital.Name),]
    
    ##define num input as the hospital rank, return the hosp name associated 
    ##within the split state
    if (num == "best") {
      return (x$Hospital.Name[1])
    }
    else if (num == "worst") {
      return (x$Hospital.Name[nrow(x)])
    }
    else { 
      return (x$Hospital.Name[num])
    }
  }, num)
  
    df <- data.frame(unlist(ans), names(ans))
    colnames(df) <- c("hospital", "state")
  
    return(df)
    ##returns the ranked hospital names and associated states in a data frame
    
}