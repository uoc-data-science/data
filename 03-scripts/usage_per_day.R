library(ggplot2)
library(ggthemes)
library(stringr)
library(plyr)
library(dplyr)
library(reshape2)

#cleanclickstream <- read.csv("02-clean-data/clickstream/clickstream_data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

a <- select(cleanclickstream, "REQUEST_DAY_OF_WEEK", "Request.Date_Time")
a$`Hour` <- substr(a$`Request.Date_Time`, 1, 2)
a <- select(a, "REQUEST_DAY_OF_WEEK", "Hour")

monday <- a[a$REQUEST_DAY_OF_WEEK == "Monday",]
tuesday <- a[a$REQUEST_DAY_OF_WEEK == "Tuesday",]
wednesday <- a[a$REQUEST_DAY_OF_WEEK == "Wednesday",]
thursday <- a[a$REQUEST_DAY_OF_WEEK == "Thursday",]
friday <- a[a$REQUEST_DAY_OF_WEEK == "Friday",]
saturday <- a[a$REQUEST_DAY_OF_WEEK == "Saturday",]
sunday <- a[a$REQUEST_DAY_OF_WEEK == "Sunday",]

monday <- count(monday[,2])
colnames(monday) <- c("Hour", "AmountMonday")
tuesday <- count(tuesday[,2])
colnames(tuesday) <- c("Hour", "AmountTuesday")
wednesday <- count(wednesday[,2])
colnames(wednesday) <- c("Hour", "AmountWednesday")
thursday <- count(thursday[,2])
colnames(thursday) <- c("Hour", "AmountThursday")
friday <- count(friday[,2])
colnames(friday) <- c("Hour", "AmountFeitag")
saturday <- count(saturday[,2])
colnames(saturday) <- c("Hour", "AmountSaturday")
sunday <- count(sunday[,2])
colnames(sunday) <- c("Hour", "AmountSunday")

daily <- left_join(monday, tuesday, "Hour")
daily <- left_join(daily, wednesday, "Hour")
daily <- left_join(daily, thursday, "Hour")
daily <- left_join(daily, friday, "Hour")
daily <- left_join(daily, saturday, "Hour")
daily <- left_join(daily, sunday, "Hour")

colnames(daily) <- c("Hour", "AmountMonday", "AmountTuesday", "AmountWednesday", "AmountThursday","AmountFriday", "AmountSaturday", "AmountSunday")

#ggplot(data = daily, aes(group=1, scale_fill_grey(start = 0, end = 1))) + 
#  geom_line(aes(x = daily$Hour, y = AmountMonday, color='Monday'), size=1, linetype = "dotted") +
#  geom_line(aes(x = daily$Hour, y = AmountTuesday, color='Tuesday'), size=1, linetype = "dotted") +
# geom_line(aes(x = daily$Hour, y = AmountWednesday, color='Wednesday'), size=1, linetype = "dotted") +
#  geom_line(aes(x = daily$Hour, y = AmountThursday, color='Thursday'), size=1, linetype = "dotted") +
#  geom_line(aes(x = daily$Hour, y = AmountFriday, color='Friday'), size=2, linetype = "solid") +
#  geom_line(aes(x = daily$Hour, y = AmountSaturday, color='Saturday'), size=2, linetype = "solid") +
#  geom_line(aes(x = daily$Hour, y = AmountSunday, color='Sunday'), size=2, linetype = "solid") +
#  scale_color_manual(values = c(
#  'Monday' = 'black',
#  'Tuesday' = 'gray30',
#  'Wednesday' = 'gray60',
#  'Thursday' = 'gray80',
#  'Friday' = 'black',
#  'Saturday' = 'black',
#  'Sunday' = 'gray60')) +
#  labs(title="Time of Visits", subtitle= paste("Out of", nrow(cleanclickstream), "site visits")) +
#  theme(axis.text.x = element_text(angle=70, vjust=1, hjust = 1))+
# xlab('Daytime') +
#  ylab('Visitors')
