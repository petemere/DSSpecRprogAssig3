rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", 
                                colClasses = "character")

        # Extract only the interesting columns
        outcomeData <- cbind(outcomeData[2], outcomeData[7], outcomeData[11],
                               outcomeData[17], outcomeData[23])

        ## Check that state and outcome are valid
        validOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
        # Outcomes in columns 11, 17, and 23.

        # Split it by state
        outcomeData <- split(outcomeData, outcomeData$State)

        outcomeIndex <- max(c(3:5) * (validOutcomes == outcome))
        stateIndex <- max(c(1:length(names(outcomeData))) *
                                  (names(outcomeData) == state))
        if (num == 'best') {
                num <- 1
        }

        # Validate the parameters
        if (stateIndex == 0) {
                stop('invalid state')
        }
        if (outcomeIndex == 0) {
                stop('invalid outcome')
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        HospRanks <- order(as.numeric(outcomeData[[stateIndex]][[outcomeIndex]]), 
                           outcomeData[[stateIndex]]$Hospital.Name, na.last = NA)
        
        rankedHospitals <- rbind(outcomeData[[stateIndex]][[outcomeIndex]], 
                                 outcomeData[[stateIndex]]$Hospital.Name)[,HospRanks]
        
        worstRank <- dim(rankedHospitals)[2]
        if (num == 'worst') num <- worstRank
        if (num > worstRank) NA else rankedHospitals[2, num]  
        
        
        
}