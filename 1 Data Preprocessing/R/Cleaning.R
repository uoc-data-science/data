pathOrders <- "../../orders/order_data.csv"
newPathOrders <- "../../0 Data/order_data_R.csv"
pathClicks <- "../../clickstream/clickstream_data.csv"
newPathClicks <- "../../0 Data/clickstream_data_R.csv"
pathOrdersClean <- "../../0 Data/order_data_cleaned_R.csv"
pathOrderHeaders <- "../../orders/order_columns.txt"
pathClicksClean  <- "../../0 Data/clickstream_data_cleaned_R.csv"
pathClicksHeaders <- "../../clickstream/clickstream_columns.txt"

# 1) Copy csv to data folder
file.copy(pathClicks, newPathClicks, overwrite = TRUE )
file.copy(pathOrders, newPathOrders, overwrite = TRUE )
pathOrders <- newPathOrders
pathClicks <- newPathClicks

# 2) read files, set headers and replace ? with NA

# Function to get headers from header.txt
getHeaders = function(filepath) {
    headers <- list()
    i <-1
    con <- file(filepath, "r")
    while ( TRUE ) {
        line <- readLines(con, n = 1)
        header <- gsub(":.*$","",line)
        if ( length(line) == 0 ) {
            break
        }
        headers[[i]] <- header
        i <- i+1
    }
    close(con)
    return(headers)
}

# get headers in an array
orderHeaders <- getHeaders(pathOrderHeaders)
clickHeaders <- getHeaders(pathClicksHeaders)

# read files, set headers and replace ? with NA
orders <- read.csv(file=pathOrders, header=FALSE)
orders[orders=="?"]<-NA
colnames(orders) <- orderHeaders
# save as new csv
write.table(orders, file = pathOrdersClean, sep=",", row.names=FALSE)
clicks <- read.csv(file=pathClicks, header=FALSE)
clicks[clicks=="?"]<-NA
colnames(clicks) <- clickHeaders
write.table(clicks, file = pathClicksClean, sep=",", row.names=FALSE)