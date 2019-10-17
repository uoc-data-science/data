# Joins a CSV data file with its corresponding column names and returns the DataFrame #
readdatawithcolumns <- function(headersFile, dataFile) {
    message("Reading headers...")
    tmpHeaderNames <- read.csv(file=headersFile, sep=":", header=FALSE)
    headerNames <- tmpHeaderNames$V1
    str(headerNames)

    message("Replacing spaces...")
    headerNames <- gsub(" ", "_", headerNames)
    str(headerNames)

    message("Adding headers to data file...")
    df <- read.csv(file=dataFile, sep=",", header=FALSE)
    colnames(df) <- headerNames
    return(df)
}

# read the ORDER data and write the dataset that contains proper headers to data/interim/:
orderDf <- readdatawithcolumns(headersFile="data/raw/orders/order_columns.txt", dataFile="data/raw/orders/order_data.csv")
write.csv(orderDf, file="data/interim/orders/orders_with_headers.csv", row.names=FALSE)
message("Done!")

# read the CLICKSTREAM data and write the dataset that contains proper headers to data/interim/:
clickstreamDf1 <- readdatawithcolumns(headersFile="data/raw/clickstream/clickstream_columns.txt",
                                      dataFile="data/interim/clickstream/clickstream_data.csv")
clickstreamDf2 <- readdatawithcolumns(headersFile="data/raw/clickstream/clickstream_columns.txt",
                                      dataFile="data/raw/clickstream/clickstream_data_part_2.csv")
clickstreamDf <- rbind(clickstreamDf1, clickstreamDf2)
write.csv(clickstreamDf, file="data/interim/clickstream/clickstream_with_headers.csv", row.names=FALSE)
message("Done!")
