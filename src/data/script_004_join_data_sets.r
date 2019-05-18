library(dplyr)

message("Reading orders data...")
orderDf <- read.csv(file="data/interim/orders/orders_with_headers.csv", sep=",")
partialOrdersDf <- orderDf[c("Order_Session_ID", "Product_Object_ID", "Product_ID")]

# force certain variables to be categorical:
partialOrdersDf$Order_Session_ID <- factor(partialOrdersDf$Order_Session_ID)
partialOrdersDf$Product_Object_ID <- factor(partialOrdersDf$Product_Object_ID)
summary(partialOrdersDf)

message("Reading clickstream data...")
clickstreamDf <- read.csv(file="data/interim/clickstream/clickstream_with_headers.csv", sep=",")
partialClickstreamDf <- clickstreamDf[c("Session_ID", "Product_Object_ID", "Product_ID", "Request_Date")]

# force certain variables to be categorical:
partialClickstreamDf$Session_ID <- factor(partialClickstreamDf$Session_ID)
partialClickstreamDf$Product_Object_ID <- factor(partialClickstreamDf$Product_Object_ID)
summary(partialClickstreamDf)



# Daten joinen ("all.x=TRUE" -> left outer join):
message("Joining data...")
joinedDf <- dplyr::left_join(partialClickstreamDf, partialOrdersDf, by=c('Session_ID'='Order_Session_ID',
        'Product_Object_ID'='Product_Object_ID',
        'Product_ID'='Product_ID'))
message("Done!")

message("SUMMARY")
summary(joinedDf)

message("HEAD")
head(joinedDf)

message("Writing the joined data frame to disc...")
write.csv(joinedDf, file="data/interim/joined/joined_df.csv", row.names=FALSE)
message("Done!")
