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
# 2) Replace all ; with ,
original <- readLines(pathOrders)
replaceSeparator <- gsub(pattern = ";", replace = ",", x=original)
writeLines(replaceSeparator, con=pathOrders)

original <- readLines(pathClicks)
replaceSeparator <- gsub(pattern = ";", replace = ",", x=original)
writeLines(replaceSeparator, con=pathClicks)

# 3) read files, select subset and set headers

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

orderHeaders <- getHeaders(pathOrderHeaders)
clickHeaders <- getHeaders(pathClicksHeaders)

# read files, select subset and set headers
orders <- read.csv(file=pathOrders, header=FALSE)
# Remove useless columns
orders <- orders[,c(1:58)]
orderHeaders <- orderHeaders[1:58]
colnames(orders) <- orderHeaders
# save as new csv
write.table(orders, file = pathOrdersClean, sep=",", row.names=FALSE)
# clicks <- read.csv(file=pathClicks, header=FALSE, fileEncoding="us-ascii")
clicks <- read.csv(file=pathClicks, header=FALSE)
# Remove useless columns
clicks <- clicks[,c(1:106)]
clickHeaders <- clickHeaders[1:106]
colnames(clicks) <- clickHeaders
write.table(clicks, file = pathClicksClean, sep=",", row.names=FALSE)