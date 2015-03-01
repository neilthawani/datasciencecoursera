complete <- function(directory, id = 1:332) {
	dir = paste(getwd(), "/", directory, sep="")
	files <- list.files(dir)
	dataList <- lapply(paste(dir, "/", files, sep=""), read.csv)
	allData <- do.call(rbind, dataList)
	
	dataWithoutNas <- allData[complete.cases(allData), ]

	colClasses <- c("numeric", "numeric")
	col.names <- c("", "")

	completed <- read.table(text = "", colClasses = colClasses, col.names = col.names)

	for (val in id) {
		subsettedData <- dataWithoutNas[dataWithoutNas$ID == val,]
		completed <- rbind(completed, c(val, nrow(subsettedData)))
	}

	colnames(completed) <- c("id", "nobs")
	completed
}

