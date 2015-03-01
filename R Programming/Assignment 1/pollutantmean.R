pollutantmean <- function(directory, pollutant, id = 1:332) {
	dir = paste(getwd(), "/", directory, sep="")
	files <- list.files(dir)
	dataList <- lapply(paste(dir, "/", files, sep=""), read.csv)
	allData <- do.call(rbind, dataList)
	limitedFrame <- subset(allData, ID %in% id)
	avg <- mean(limitedFrame[, pollutant], na.rm = TRUE)
}


