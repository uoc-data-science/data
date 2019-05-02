# Path of original file
pathOrders <- "orders/order_data.csv"
# Path of original file to be pasted into the Data folder
newPathOrders <- "0 Data/order_data_R.csv"
# Path of the cleaned file
pathOrdersClean <- "0 Data/order_data_cleaned_R.csv"
# Path of the small version of the clean file
pathOrdersSmall = "0 Data/order_data_small_R.csv"
# Path of the headers
pathOrderHeaders <- "orders/order_columns.txt"

pathClicks <- "clickstream/clickstream_data.csv"
newPathClicks <- "0 Data/clickstream_data_R.csv"
pathClicksClean  <- "0 Data/clickstream_data_cleaned_R.csv"
pathClicksSmall = "0 Data/clickstream_data_small_R.csv"
pathClicksHeaders <- "clickstream/clickstream_columns.txt"

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
orders[orders=="NULL"]<-NA
colnames(orders) <- orderHeaders
# drop columns which have only NA values
orders <- orders[,colSums(is.na(orders))<nrow(orders)]
# save as new csv
write.table(orders, file = pathOrdersClean, sep=",", row.names=FALSE)

clicks <- read.csv(file=pathClicks, header=FALSE)
clicks[clicks=="?"]<-NA
clicks[clicks=="NULL"]<-NA
colnames(clicks) <- clickHeaders
clicks <- clicks[,colSums(is.na(clicks))<nrow(clicks)]
write.table(clicks, file = pathClicksClean, sep=",", row.names=FALSE)

# 3) Save smaller versions for readability and dev purposes
small_size = min(10000,nrow(orders))
orders_small = orders[1:small_size,]
write.table(orders_small, file = pathOrdersSmall, sep=",", row.names=FALSE)

small_size = min(10000,nrow(clicks))
clicks_small = clicks[1:small_size,]
write.table(clicks_small, file = pathClicksSmall, sep=",", row.names=FALSE)