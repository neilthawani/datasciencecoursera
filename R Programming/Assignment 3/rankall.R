rankall <- function(outcome, num = "best") {
    passedInNum <- num
    # read outcome data
    outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses="character")

    names <- c("pneumonia", "heart attack", "heart failure")
    if (!(TRUE %in% grepl(tolower(outcome), tolower(names)))) {
        stop("invalid outcome")
    } else {
        colNamePrefix <- "Hospital 30 Day Death  Mortality  Rates from"
        colName <- paste(colNamePrefix, outcome)

        names <- gsub("[.]", " ", names(outcome_data))
        colIndex <- match(tolower(colName), tolower(names))

        # sort by rank
        outcome_data[, colIndex] <- suppressWarnings(as.numeric(outcome_data[, colIndex], na.rm = TRUE))
        sorted_outcome_data <- outcome_data[order(outcome_data[, 7], outcome_data[, colIndex], outcome_data[, 2]), ]
        
        state_vector <- sort(unique(outcome_data[, 7]))
        retDf <- data.frame(matrix(nrow = 0, ncol = 46))
        for (state in state_vector) {
            # for each state, find the hospital of the given rank
            stateDf <- sorted_outcome_data[sorted_outcome_data[, 7] == state, ]
            stateDf <- stateDf[complete.cases(stateDf), ]
            
            if (passedInNum == "best") {
                num <- 1
            } else if (passedInNum == "worst") {
                num <- nrow(stateDf)
                
            }
            
            retDf[nrow(retDf) + 1, ] <- stateDf[num, ]
            retDf[nrow(retDf), 7] <- state
        }
        
        colOne <- retDf[2]
        colTwo <- retDf[7]
        colOneName <- "hospital"
        colTwoName <- "state"
        
        retVal <- data.frame(colOne, colTwo)
        names(retVal) <- c(colOneName, colTwoName)

        # return a data frame with the hospital names and the (abbreviated) state name
        retVal
    }
}