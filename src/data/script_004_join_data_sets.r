library(dplyr)

message("Reading clickstream data...")
clickstreamDf <- read.csv(file="data/interim/clickstream/clickstream_with_headers.csv", sep=",")
partialClickstreamDf <- clickstreamDf[c("Session_ID", "Product_Object_ID",
                                        "Product_ID", "Request_Date", "Request_Sequence")]

# force certain variables to be categorical:
partialClickstreamDf$Session_ID <- factor(partialClickstreamDf$Session_ID)
partialClickstreamDf$Product_Object_ID <- factor(partialClickstreamDf$Product_Object_ID)
partialClickstreamDf$Request_Sequence <- factor(partialClickstreamDf$Request_Sequence)
summary(partialClickstreamDf)


message("Reading orders data...")
orderDf <- read.csv(file="data/interim/orders/orders_with_headers.csv", sep=",")
partialOrdersDf <- orderDf[c("Order_Session_ID", "Order_Line_Session_ID",
                             "Product_Object_ID", "Product_ID")]

# force certain variables to be categorical:
partialOrdersDf$Order_Session_ID <- factor(partialOrdersDf$Order_Session_ID)
partialOrdersDf$Product_Object_ID <- factor(partialOrdersDf$Product_Object_ID)
#Order_Line_Session_ID <- factor(Order_Line_Session_ID)
summary(partialOrdersDf)


# join data:
message("\n\n\nJoining data...")
joinedDf <- partialClickstreamDf %>%
  inner_join(partialOrdersDf, by=c('Session_ID'='Order_Line_Session_ID'
#                                                                   'Session_ID'='Order_Session_ID',
#                                                                   'Product_ID'='Product_ID',
                                                                   ))
message("Done!")

message("\n\n\nSUMMARY")
summary(joinedDf)

message("HEAD")
head(joinedDf)

message("Writing the joined data frame to disc...")
#write.csv(joinedDf, file="data/interim/joined/joined_df.csv", row.names = TRUE)
message("Done!")
