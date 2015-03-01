rankhospital <- function(state, outcome, num = "best") {
    # read outcome data
    outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses="character")
    
    # check that state and outcome are valid
    if (!(state %in% unique(outcome_data[, 7]))) {
        stop("invalid state")
    } else {
        state_outcome_data <- outcome_data[outcome_data[, 7] == state, ]
    }
    
    names <- c("pneumonia", "heart attack", "heart failure")
    if (!(TRUE %in% grepl(tolower(outcome), tolower(names)))) {
        stop("invalid outcome")
    } else {
        colNamePrefix <- "Hospital 30 Day Death  Mortality  Rates from"
        colName <- paste(colNamePrefix, outcome)
        
        names <- gsub("[.]", " ", names(state_outcome_data))
        colIndex <- match(tolower(colName), tolower(names))
        state_outcome_data[, colIndex] <- suppressWarnings(as.numeric(state_outcome_data[, colIndex], na.rm = TRUE))
        sortedDf <- state_outcome_data[order(state_outcome_data[, colIndex], state_outcome_data[, 2]), ]
        sortedDf <- sortedDf[!is.na(sortedDf[, colIndex]), ]
        
        
        # return hospital name in that state with the given rank
        if (num == "worst") {
            retVal <- sortedDf[nrow(sortedDf), 2]
        } else if (num == "best") {
            retVal <- sortedDf[1, 2]
        } else {
            retVal <- sortedDf[num, 2]
        }
        
        # 30-day death rate
        retVal
    }
}