# dec=".", quote="\"", comment.char=""


#load the order CSV into R-Studio in-memory-array:
#if the first row of this csv file is a list of column names. We used the header=TRUE argument
orders <- read.csv("01-raw-data/orders/order_data.csv", header=FALSE, sep=",")
orders_columns <-read.delim("01-raw-data/orders/order_columns.txt", header=FALSE, sep="\n")

#cut all posible values behind the column name
for (this_column in orders_columns)
{
  #print(this_column)
  #this_column <- gsub(":*", "", this_column)
  print(gsub(":*", "", this_column))
}

# dec=".", quote="\"", comment.char=""
# Fixme:
# load the clickstream.csv into R-Studio in-memory-array:
#clickstream <- read.table("01-raw-data/clickstream/clickstream_data.csv", header=TRUE, sep=",")



#Todo:
# Write cleaned CSV back to \Cleaned\...
#write.csv(orders, file = '02-clean-data/orders/order_data.csv')
