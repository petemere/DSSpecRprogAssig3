rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", 
                                colClasses = "character")
        
        # Extract only the interesting columns
        # Hospital name in column 2, state in 7, and outcomes in 11, 17, and 23.
        outcomeData <- cbind(outcomeData[2], outcomeData[7], outcomeData[11],
                             outcomeData[17], outcomeData[23])
        
        ## Set valid outcomes
        validOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
        
        # Split it by state
        outcomeData <- split(outcomeData, outcomeData$State)
        
        ## Check that outcome is valid and set num to 1 if == 'best'
        thisNum <- num
        if (num == 'best') thisNum <- 1
        outcomeIndex <- max(c(3:5) * (validOutcomes == outcome))
        if (outcomeIndex == 0) stop('invalid outcome')
        
        
        ## For each state, find the hospital of the given rank
        ## 30-day death rate
        rankedHospitalPerState <- data.frame(
                hospital = character(length(outcomeData)), 
                state = character(length(outcomeData)),
                stringsAsFactors = FALSE)
        rownames(rankedHospitalPerState) <- names(outcomeData)
        
        i <- 1
        for (stateData in outcomeData) {
                stateData <- as.data.frame(stateData)
                HospRanks <- order(as.numeric(stateData[[outcomeIndex]]), 
                                   stateData[1], na.last = NA)

                rankedHospitalPerState$state[i] <- stateData[[2]][1] # The state ID
                rankedHospitals <- stateData[[1]][HospRanks]
                
                ## Add the appropriate hospital name to the data frame.
                worstRank <- length(HospRanks)
                if (num == 'worst') thisNum <- worstRank
                thisHospital <- if (thisNum > worstRank) NA else rankedHospitals[thisNum]  
                rankedHospitalPerState$hospital[i] <- thisHospital
                
                i <- i + 1
        }
        
        rankedHospitalPerState
}