library(ggplot2)
library(usmap)
library(dplyr)
library(sqldf)

cleanorders <- read.csv("02-clean-data/orders/order_data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

unique_orders <- distinct(cleanorders, Order.Customer.ID, .keep_all = TRUE)

a <- count(unique_orders$`US.State`)
colnames(a) <- c("State", "Sells")
a <- transform(a, Percentage = Sells*100/sum(a$"Sells"))

states <- read.csv("01-raw-data/states.csv", header=FALSE, sep=",", stringsAsFactors=FALSE)
colnames(states) <- c("Statename", "State", "Population")

a <- left_join(a, states, by = "State", match = "all")
a <- transform(a, PopRatio = Population*100/sum(a$Population))

a <- a[order(a$Sells, decreasing = TRUE), ]
a$Statename <- factor(a$Statename, levels = a$Statename)

b <- select(a, "State", "Percentage")
colnames(b) <- c("state", "percentage")

plot_usmap(data = b, values = "percentage", lines = "black") + 
  scale_fill_continuous(
    low = "grey", high = "black", name = "Percentage of all sales", label = scales::comma
  ) +
  labs(title="Geographical distribution of sales", 
       subtitle=paste("Out of", nrow(cleanorders), "sales")) +
  theme(legend.position = "right")