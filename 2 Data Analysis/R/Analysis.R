useSmallVersions <- FALSE

pathOrders <- "0 Data/order_data_cleaned_R.csv"
pathClicks  <- "0 Data/clickstream_data_cleaned_R.csv"
if (useSmallVersions){
    pathOrders <- "0 Data/order_data_small_R.csv"
    pathClicks <- "0 Data/clickstream_data_small_R.csv"
}
pathOrdersPython <- "0 Data/order_data_cleaned_P.csv"
pathClicksPython  <- "0 Data/clickstream_data_cleaned_P.csv"
if (useSmallVersions){
  pathOrdersPython <- "0 Data/order_data_small_P.csv"
  pathClicksPython <- "0 Data/clickstream_data_small_P.csv"
}

pathNULLAnalysisOrders <- "2 Data Analysis/NAorders.csv"
pathNULLAnalysisClicks <- "2 Data Analysis/NAclicks.csv"

orders <- read.csv(file=pathOrders)
clicks <- read.csv(file=pathClicks)

# Create a DF to save the column names and appropriate NA percentage values
NAorders <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Column_Name", "NA_percentage")
colnames(NAorders) <- x

# Get the NA percentage errors
for (column in names(orders)){
    percentageNA <- round(sum(is.na(orders[,column]))/length(orders[,column]),digits=4)
    NAorders[nrow(NAorders) + 1,] = list(column,percentageNA)
}
# Sort by NA percentage ascending
NAorders <- NAorders[with(NAorders, order(NAorders$NA_percentage)),]
write.table(NAorders, file = pathNULLAnalysisOrders, sep=",", row.names=FALSE)


# Create a DF to save the column names and appropriate NA percentage values
NAclicks <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Column_Name", "NA_percentage")
colnames(NAclicks) <- x

# Get the NA percentage errors
for (column in names(clicks)){
    percentageNA <- round(sum(is.na(clicks[,column]))/length(clicks[,column]),digits=4)
    NAclicks[nrow(NAclicks) + 1,] = list(column,percentageNA)
}
# Sort by NA percentage ascending
NAclicks <- NAclicks[with(NAclicks, order(NAclicks$NA_percentage)),]
write.table(NAclicks, file = pathNULLAnalysisClicks, sep=",", row.names=FALSE)

# Compare csv files
library(diffobj)
clicksP = read.csv(file=pathClicksPython,na.strings=c("","NA"))
diffPrint(target=clicksP,current=clicks)
ordersP = read.csv(file=pathOrdersPython,na.strings=c("","NA"))
diffPrint(target=ordersP,current=orders)