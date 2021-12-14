##Finding the best hospital in the state

best <- function(state,outcome) 
{
  ##read outcome data:
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##check that state and outcome data are valid
  if (!any(state == file$State)){
    stop("invalid state") }
  ##print "invalid state" if the state input doesn't match any State listed in 
  ##the source file
  if ((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE) {
    stop("invalid outcome") }
  ##print "invalid outcome" if the outcome input doesn't match any condition
  ##listed in the listed vector

  state_subset <- subset(file, State == state) ##subset the data frame to the 
  ##specified state

  ##specify column number based on outcome (i.e. medical condition) entered:
  if (outcome == "heart attack") { 
    columnNo <- 11 }
  if (outcome == "heart failure") {
    columnNo <- 17 }
  else (columnNo <- 23)

##print hospital name associated with minimum deaths in specified state and 
##outcome:
min_row <- which(as.numeric(state_subset[ ,columnNo]) == 
                   min(as.numeric(state_subset[ ,columnNo]),na.rm = TRUE))
                 ##lists the row with min deaths, skipping NA values
                 hospital_names <- state_subset[min_row,2] ##list of hospital names with min deaths
                 hospitals <- sort(hospital_names) ##sorting hospitals alphabetically
                 print(hospitals[1]) ##returns the name of the first hospital in the list
}

##Committer: Kaylyn Gillespie <kaylyngillespie@Kaylyns-MacBook-Pro.local>
##Your name and email address were configured automatically based
##on your username and hostname. Please check that they are accurate.
##You can suppress this message by setting them explicitly. Run the
##following command and follow the instructions in your editor to edit
##your configuration file:
  
##  git config --global --edit

##After doing this, you may fix the identity used for this commit with:
  
##  git commit --amend --reset-author

##1 file changed, 1 insertion(+), 1 deletion(-)