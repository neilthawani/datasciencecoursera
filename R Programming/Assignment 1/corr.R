corr <- function(directory, threshold = 0) {
	dir = paste(getwd(), "/", directory, sep="")
	files <- list.files(dir)
	dataList <- lapply(paste(dir, "/", files, sep=""), read.csv)
	allData <- do.call(rbind, dataList)

	completedData <- complete(directory)

	thresholdData <- completedData[completedData$nobs > threshold, ]

	retVal <- numeric(0)


	for (id in thresholdData$id) {
		validRows <- allData[!is.na(allData$sulfate) & !is.na(allData$nitrate) & allData$ID == id, ]
		retVal <- append(retVal, cor(validRows$sulfate, validRows$nitrate))
	}

	retVal
}
