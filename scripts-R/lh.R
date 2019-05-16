
#import order data heads
order_columns <- file("C:/Github/Programming_Data_Science/ourdata/01_data_understanding/order_columns.txt","r")
order_heads <- c()
while (TRUE){
  line = readLines(order_columns,n=1)
  if(length(line)==0)
    {
    break
    }
  order_heads <-append(order_heads,strsplit(line,":")[[1]][1])
}

#import order data with heads via order_heads
order_data <- read.csv("C:/Github/Programming_Data_Science/ourdata/00_raw_data/order_data.csv", col.names= order_heads,encoding = "ansi")

#replace ? and NULL with NA
library(naniar)
na_strings <- c("?","NULL")
order_data <- order_data %>% replace_with_na_all(condition = ~.x %in% na_strings)

head(order_data)

#clean order 
order_data[colSums(!is.na(order_data)) > 0]

summary(order_data)



#plot with ggplot2
library(tidyverse)
ggplot(data = order_data)+
  geom_point(mapping = aes(x=Order.Line.Quantity,
                           y=Order.Amount, 
                           color=Gender))+
  facet_wrap(~Order.Credit.Card.Brand,nrow = 2)
  


#plot with ggplot2
ggplot(data = order_data)+
  geom_point(mapping = aes(x=Order.Line.Quantity,
                           y=Order.Amount, 
                           color=Gender))


#plot with ggplot2
ggplot(data = order_data)+
  geom_point(mapping = aes(x=Customer.ID,
                           y=Order.Amount, 
                           color=Order.Line.Day.of.Week))

#plot with ggplot2
ggplot(data = order_data)+
  geom_point(mapping = aes(x=Order.Amount,
                           y=Product.ID))


#plot with ggplot2
ggplot(data = order_data)+
  geom_point(mapping = aes(x=Order.Line.Status,
                           y=Order.Amount,
                           color=Order.Amount))+
  facet_wrap(~Gender,nrow = 2)

             