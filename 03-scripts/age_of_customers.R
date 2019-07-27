library(ggplot2)
library(dplyr)
library(sqldf)
library(tidyr)
library(reshape2)

cleanorders <- read.csv("02-clean-data/orders/order_data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

unique_customers <- distinct(cleanorders, Order.Customer.ID, .keep_all = TRUE)

customers <- select(unique_customers, "Age", "Gender")
men <- filter(customers, customers$Gender == "Male", customers$'Age' != "?")
n_men <- nrow(men)
women <- filter(customers, customers$Gender == "Female", customers$'Age' != "?")
n_women <- nrow(women)
nogender <- filter(customers, customers$Gender != "Male" & customers$Gender != "Female", customers$'Age' != "?")
all <- filter(customers, customers$Gender == "Male" | customers$Gender == "Female", customers$'Age' != "?")
n_all <- nrow(all)

men <- count(men$Age)
colnames(men) <- c("Age", "AmountMen")
women <- count(women$Age)
colnames(women) <- c("Age", "AmountWomen")
nogender <- count(nogender$Age)
colnames(nogender) <- c("Age", "AmountNoAnswer")
all <- count(all$Age)
colnames(all) <- c("Age", "AmountAll")

a <- left_join(all, men, by = "Age", match = "all")
a <- left_join(a, women, by = "Age", match = "all")
a <- left_join(a, nogender, by = "Age", match = "all")

#ggplot() + geom_line(linetype = "dashed")+
#  geom_line(aes(x=a$'Age',y=a$'AmountAll', color='All'), group = 1, size=1.5, linetype = "solid") +
#  geom_line(aes(x=a$'Age',y=a$'AmountMen', color='Men'), group = 1, size=1.01, linetype = "longdash") +
#  geom_line(aes(x=a$'Age',y=a$'AmountWomen', color='Women'), group = 1, size=1.01, linetype = "twodash") +
#  geom_line(aes(x=a$'Age',y=a$'AmountNoAnswer', color='No Answer'), group = 1, size=1.01, linetype = "dotdash") +
#  scale_color_manual(values = c(
#    'All' = 'black',
#    'Men' = 'gray20',
#    'Women' = 'gray80')) +
#  ylab('Amount of Customers')+xlab('Age')+
#  labs(title="Age of Customers", 
#       subtitle=paste("Out of", nrow(all), "sales")) 