library(janitor)

# load csv files into dataframe
cleanFile <- function(filePath, threshold) {
  message(sprintf("Loading file from %s.", filePath))
  df <- read.csv(file=filePath, header = TRUE)
  
  message("Cleaning data.")
  message("1. Removing empty rows and columns.")
  dfCleaned <- remove_empty(dat = df, which = c("rows", "cols"))
  columnsDeleted <- ncol(df) - ncol(dfCleaned)
  message(sprintf("1. Removed %d columns", columnsDeleted))
  
  
  message("2. Removing constant columns.")
  dfCleaned <- remove_constant(dat = dfCleaned, na.rm = TRUE)
  columnsDeleted <- ncol(df) - ncol(dfCleaned) - columnsDeleted
  message(sprintf("2. Removed %d columns", columnsDeleted))
  
  message(sprintf("3. Removing columns with unknown values (threshold = %f).", threshold))
  deletableColumns <- list()
  for(c in 1:ncol(dfCleaned)){
    currentColumn <- dfCleaned[,c]
    unknownPercentage <- sum(currentColumn == "NULL" | currentColumn == "?")/length(currentColumn)
    if(unknownPercentage > threshold){
      deletableColumns <- c(deletableColumns, colnames(dfCleaned)[c])
    }
  }
  
  columnsDeleted <- length(deletableColumns)
  message(sprintf("3. Removed %d columns", columnsDeleted))
  
  dfCleaned <- dfCleaned[, !(names(dfCleaned) %in% deletableColumns)]
  
  message(sprintf("File cleaned. Total deleted columns: %d", ncol(df)-ncol(dfCleaned)))
  return(dfCleaned)
}

clickstreamDfCleaned <- cleanFile("data/interim/clickstream/clickstream_with_headers.csv", 0.9)
write.csv(clickstreamDfCleaned, file="data/interim/clickstream/clickstream_cleaned.csv", row.names=FALSE)

orderDfCleaned <- cleanFile("data/interim/orders/orders_with_headers.csv", 0.9)
write.csv(orderDfCleaned, file="data/interim/orders/orders_cleaned.csv", row.names=FALSE)
