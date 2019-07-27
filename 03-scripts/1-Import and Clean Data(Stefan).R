# dec=".", quote="\"", comment.char=""

#prepare workspace by deleting data from environment
rm(list=ls())
library(plyr)
#load the order CSV into R-Studio in-memory-array:
orders <- read.csv("01-raw-data/orders/order_data.csv", header=FALSE, sep=",", stringsAsFactors=FALSE)
orders_columns <-read.delim("01-raw-data/orders/order_columns.txt", header=FALSE, sep="\n")

#load the clickstream CSV into R-Studio in-memory-array
if(file.exists("01-raw-data/clickstream/clickstream_data.csv")){
  clickstream <- read.csv("01-raw-data/clickstream/clickstream_data.csv", header=FALSE, sep=",", stringsAsFactors=FALSE)
}
if(!file.exists("01-raw-data/clickstream/clickstream_data.csv")){
  if(file.exists("01-raw-data/clickstream/clickstream_data.zip")){
    clickstream <- read.csv(unzip("01-raw-data/clickstream/clickstream_data.zip", "01-raw-data/clickstream/clickstream_data.csv"), header=FALSE, sep=",", stringsAsFactors=FALSE)
  }
}

if(file.exists("01-raw-data/clickstream/clickstream_data_part_2.csv")){
clickstream2 <- read.csv("01-raw-data/clickstream/clickstream_data_part_2.csv", header=FALSE, sep=",", stringsAsFactors=FALSE)
}
clickstream_columns <-read.delim("01-raw-data/clickstream/clickstream_columns.txt", header=FALSE, sep="\n")

#load the experiment CSV into R-Studio in-memory-array
experiment <- read.csv("01-raw-data/experiment/experimental_results.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
#

rowType_orders <- NULL
rowType_clickstream <- NULL
rowType_clickstream2 <- NULL

######
###cut all posible values behind the column name and rename the columns names in the orders matrix
###Save allowed values per row to a vector  
######
# Orders:
for (i in 1:nrow(orders_columns)) {
  rowType_orders <- c(rowType_orders, as.character(orders_columns[i,1]))
  colnames(orders)[i] <- sub(':.*', '', orders_columns[i,1])
}
# Clickstream:
for (i in 1:nrow(clickstream_columns)) {
  rowType_clickstream <- c(rowType_clickstream, as.character(clickstream_columns[i,1]))
  colnames(clickstream)[i] <- sub(':.*', '', clickstream_columns[i,1])
}
merged_clickstream <- clickstream
# Second clickstream file:
if(file.exists("01-raw-data/clickstream/clickstream_data_part_2.csv")){
for (i in 1:nrow(clickstream_columns))
{
  rowType_clickstream2 <- c(rowType_clickstream2, as.character(clickstream_columns[i,1]))
  colnames(clickstream2)[i] <- sub(':.*', '', clickstream_columns[i,1])
}

# Merge both clickstream files and write them into merged_clickstream.csv
merged_clickstream <-  rbind(clickstream, clickstream2)
}


# Remove unusable columns from orders

ratiobadvalues <- 90
ratiosamevalues <- 97
ratiosamevaluesandnull <- 90

deletecolumn <- vector(mode = "logical", length = ncol(orders))

for (j in 1:ncol(orders)){
  trashratio <- 0.0
  trashcounter <- sum(sapply(orders[,j], function(x) sum(x == "NULL" || x == "?" || is.na(x))))
  trashratio <- trashcounter * 100 / nrow(orders)
  
  a <- data.frame(orders[,j])
  colnames(a) <- "Column"
  a <- count(a, "Column")
  a <- a[order(a$freq, decreasing = TRUE), ]
  samevalues <- a[1,2]
  
  if(trashratio >= ratiobadvalues || samevalues*100/nrow(orders)>ratiosamevalues || (samevalues*100/nrow(orders)>ratiosamevaluesandnull && a[2,1] == "NULL")){
    deletecolumn[j] <- TRUE
  }
}
trashless_orders <- orders[,!deletecolumn]

# Remove unusable columns from clickstream

deletecolumn <- vector(mode = "logical", length = ncol(merged_clickstream))

for (j in 1:ncol(merged_clickstream)){
  trashratio <- 0.0
  trashcounter <- sum(sapply(merged_clickstream[,j], function(x) sum(x == "NULL" || x == "?" || is.na(x))))
  trashratio <- trashcounter * 100 / nrow(merged_clickstream)
  
  a <- data.frame(merged_clickstream[,j])
  colnames(a) <- "Column"
  a <- count(a, "Column")
  a <- a[order(a$freq, decreasing = TRUE), ]
  samevalues <- a[1,2]
  
  if(trashratio >= ratiobadvalues || samevalues*100/nrow(merged_clickstream)>ratiosamevalues || (samevalues*100/nrow(merged_clickstream)>ratiosamevaluesandnull & a[2,1] == "NULL")){
    deletecolumn[j] <- TRUE
  }
}
trashless_clickstream <- merged_clickstream[,!deletecolumn]

# Join of orders and clickstream

clickstreamtojoin <- trashless_clickstream
deleterow <- vector(mode = "logical", length = nrow(clickstreamtojoin))
for (i in 1:nrow(clickstreamtojoin)){
    if(clickstreamtojoin$`Session Customer ID`[i] == "?"){
    deleterow[i] <- TRUE
    }
}
joined_csv <- clickstreamtojoin[!deleterow,]
names(joined_csv)[names(joined_csv)=="Customer ID"] <- "Customer ID different"
joined_csv$`Session Customer ID` <- as.numeric(joined_csv$`Session Customer ID`)
names(joined_csv)[names(joined_csv)=="Session Customer ID"] <- "Customer ID"
joined_csv <- left_join(joined_csv, trashless_orders, by = "Customer ID", match = "all")

#Clean Time # Deletes undesired characters, Replaces unvalid time-values with NA
cleanTime <- function(file, x) {
  if (file == "orders") {
    for (i in 1:nrow(orders)) {
      orders[i,x]  <- gsub('\\\\', '', orders[i,x])
    }
    
    if (is.na(as.POSIXct(orders[i,x], format="%H:%M:%S"))) {
      orders[i,x] <- NA
    }
  }
  
  if(file == "clickstream") {
    for (i in 1:nrow(clickstream)){
      clickstream[i,x]  <- gsub('\\\\', '', clickstream[i,x])
    }
    
    if (is.na(as.POSIXct(clickstream[i,x], format="%H:%M:%S"))) {
      clickstream[i,x] <- NA
    }
  }
}

#Clean Date # Replaces unvalid date-values with NA
cleanDate <- function(file, x) {
  if(file == "orders"){
    for (i in 1:nrow(orders)) {
      if (try(class(as.Date(orders[i,x])), silent = TRUE) == "try-error") {
        orders[i,x] <- NA
      }
    }
  }
  
  if(file == "clickstream") {
    for (i in 1:nrow(clickstream)) {
      if(try(class(as.Date(clickstream[i,x])), silent = TRUE) == "try-error") {
        clickstream[i,x] <- NA
      }
    }
  }
}

#Clean URL # Deletes undesired characters, Replaces unvalid URL-addresses with NA
cleanURL <- function(file, x) {
  if(file == "orders"){
    for (i in 1:nrow(orders)) {
      orders[i,x]  <- gsub('\\\\', '', orders[i,x])
      
      if (grepl("https*[:][/][/]www[.][A-z0-9]+[.][A-z0-9]+", orders[i,x]) == FALSE) {
        orders[i,x] <- NA
      }
    }
  }
  
  if(file == "clickstream") {

    for (i in 1:10) {#nrow(clickstream)){
      print(paste(i , " " , x))
      clickstream[i,x] <- gsub('\\\\', '', clickstream[i,x])
      print(gsub('\\\\', '', clickstream[i,x]))
      #clickstream[i,x]  <- gsub('\\\\', '', clickstream[i,x])
      if (grepl("https*[:][/][/]www[.][A-z0-9]+[.][A-z0-9]+", clickstream[i,x]) == FALSE) {
        clickstream[i,x] <- NA
      }
    }
  }
}


write.csv(trashless_orders, file = '02-clean-data/orders/order_data.csv')
write.csv(trashless_clickstream, file = '02-clean-data/clickstream/clickstream_data.csv')

