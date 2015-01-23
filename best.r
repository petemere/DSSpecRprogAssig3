best <- function(state, outcome) {
        ## A function that takes two arguments:
        ##  - the 2-character abbreviated name of a state, and
        ##  - an outcome name.
        ## 
        ## The function reads the outcome-of-care-measures.csv fle and returns a
        ## character vector with the name of the hospital that has the best
        ## (i.e. lowest) 30-day mortality for the specifed outcome in that
        ## state. The hospital name is the name provided in the Hospital.Name
        ## variable. The outcomes can be one of "heart attack", "heart failure",
        ## or "pneumonia". Hospitals that do not have data on a particular 
        ## outcome should be excluded from the set of hospitals when deciding
        ## the rankings.
        ##
        ##   Handling ties.
        ## If there is a tie for the best hospital for a given outcome, then the
        ## hospital names should be sorted in alphabetical order and the frst
        ## hospital in that set should be chosen (i.e. if hospitals "b", "c",
        ## and "f" are tied for best, then hospital "b" should be returned).
        ##
        ## Check that state and outcome are valid.
        ## Return hospital name in that state with lowest 30-day death rate
        
        # Set the acceptable parameters
        validOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
                # Outcomes in columns 11, 17, and 23.

        # Open the outomes file
        outcomeData <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")

        # Extract only the interesting columns
        usefulOutcome <- cbind(outcomeData[2], outcomeData[7], outcomeData[11],
                               outcomeData[17], outcomeData[23])
        
        # Split it by state
        usefulOutcome <- split(usefulOutcome, usefulOutcome$State)
        
        # Get the indices of the parameters in the data.
        # Will be 0 if invalid outcome
        outcomeIndex <- max(c(3:5) * (validOutcomes == outcome))
        stateIndex <- max(c(1:length(names(usefulOutcome))) *
                                  (names(usefulOutcome) == state))
        
        # Validate the parameters
        if (stateIndex == 0) {
                stop('invalid state')
        }
        if (outcomeIndex == 0) {
                stop('invalid outcome')
        }

        # get the min value of the outcome we're after
        minValue <- min(as.numeric(usefulOutcome[[stateIndex]][[outcomeIndex]]),
                        na.rm = TRUE)
        
        # Get an ordered list of all hospital names that have the min value.
        allHospitals <- sort(usefulOutcome[[stateIndex]][[1]][
                as.numeric(usefulOutcome[[stateIndex]][[outcomeIndex]]) == minValue])
        
        # Return the first name in the list.
        allHospitals[1]
        
}