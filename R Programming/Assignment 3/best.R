# Finding the best hospital in a state

best <- function(state, outcome) {
	# read outcome data
	outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses="character")

	# check that state and outcome are valid
	if (!(state %in% unique(outcome_data[, 7]))) {
		stop("invalid state")
	} else {
		state_outcome_data <- outcome_data[outcome_data[, 7] == state, ]
        # sort by hospital name
        state_outcome_data <- state_outcome_data[order(state_outcome_data[, 2]), ]
	}

    names <- c("pneumonia", "heart attack", "heart failure")
	if (!(TRUE %in% grepl(tolower(outcome), tolower(names)))) {
		stop("invalid outcome")	
	} else {
        colNamePrefix <- "Hospital 30 Day Death  Mortality  Rates from"
        colName <- paste(colNamePrefix, outcome)
        names <- gsub("[.]", " ", names(state_outcome_data))
        colIndex <- match(tolower(colName), tolower(names))

        minDataForOutcome <- suppressWarnings(as.numeric(state_outcome_data[, colIndex]))
		rowIndex <- match(min(minDataForOutcome, na.rm = TRUE), minDataForOutcome)
	}

	# return hospital name in that state with the lowest 30-day death rate
	state_outcome_data[rowIndex, 2]
}
