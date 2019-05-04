library(ggplot2)
library(gridExtra)
library(dplyr)
library(janitor)

useSmallVersions <- TRUE

pathOrders <- "0 Data/order_data_cleaned_R.csv"
pathClicks  <- "0 Data/clickstream_data_cleaned_R.csv"
if (useSmallVersions){
  pathOrders <- "0 Data/order_data_small_R.csv"
  pathClicks <- "0 Data/clickstream_data_small_R.csv"
}

pathPlotFolder <- "./4 Data Overview/Plots/"
pathTableFolder <- "./4 Data Overview/Tables/"


orders <- read.csv(file=pathOrders)
clicks <- read.csv(file=pathClicks)

# Overview table of order data
interestingColumns <- c("Order.Line.Quantity",
                        "Order.Line.Unit.List.Price",
                        "Order.Line.Amount",
                        "Spend.Over..12.Per.Order.On.Average",
                        "Order.Line.Day.of.Week",
                        "Order.Line.Hour.of.Day",
                        "Order.Promotion.Code",
                        "Order.Discount.Amount"
)
summary <- summary(orders[interestingColumns])
summary[is.na(summary)]<-""
write.table(summary, file = paste(pathTableFolder,"OrderData.csv"), sep=",", row.names=FALSE)
# Overview table of payment method data
interestingColumns <- c("Order.Credit.Card.Brand",
                        "Bank.Card.Holder",
                        "Gas.Card.Holder",
                        "Upscale.Card.Holder",
                        "Unknown.Card.Type",
                        "TE.Card.Holder",
                        "Premium.Card.Holder",
                        "New.Bank.Card"
)
summary <- summary(orders[interestingColumns])
summary[is.na(summary)]<-""
write.table(summary, file = paste(pathTableFolder,"PaymentMethodData.csv"), sep=",", row.names=FALSE)
# Overview table of product data
interestingColumns <- c("StockType",
                        "Manufacturer",
                        "BrandName"
)
summary <- summary(orders[interestingColumns])
summary[is.na(summary)]<-""
write.table(summary, file = paste(pathTableFolder,"ProductData.csv"), sep=",", row.names=FALSE)
# Overview table of customer data
interestingColumns <- c("City",
                        "Country",
                        "US.State",
                        "Age",
                        "Marital.Status",
                        "Gender",
                        "Audience",
                        "Truck.Owner",
                        "RV.Owner",
                        "Motorcycle.Owner",
                        "Working.Woman",
                        "Presence.Of.Children",
                        "Speciality.Store.Retail",
                        "Oil.Retail.Activity",
                        "Bank.Retail.Activity",
                        "Finance.Retail.Activity",
                        "Miscellaneous.Retail.Activity",
                        "Upscale.Retail",
                        "Upscale.Speciality.Retail",
                        "Retail.Activity"
)
summary <- summary(orders[interestingColumns])
summary[is.na(summary)]<-""
write.table(summary, file = paste(pathTableFolder,"CustomerData.csv"), sep=",", row.names=FALSE)

# Utility function for subplot support
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      
    }
  }
}


#Plots for order data
# Distribution of order line quantity
ggplot(orders, aes(x=Order.Line.Quantity)) +
  geom_histogram(binwidth=1, colour="black", fill="white") +  # Overlay with transparent density plot
  geom_vline(aes(xintercept=mean(Order.Line.Quantity, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(name ="Order Line Quantity",limits = c(0.5,7.5),breaks = round(seq(1, 7, by = 1),1))
ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Line Quantity.png",sep=""))

# Distribution of order amount
p1 <- ggplot(orders, aes(x=Order.Line.Amount)) + 
  geom_density() +
  scale_x_continuous(name="Order Line Amount",limits = c(-0.5,45.5),breaks = round(seq(0, 45, by = 3),1)) +
  scale_y_continuous(limits = c(0,0.15))
p2 <- ggplot(orders, aes(x=Order.Line.Amount)) +
  geom_histogram(binwidth=1, colour="black", fill="white") +
  scale_x_continuous(name="Order Line Amount",limits = c(-2.5,45.5),breaks = round(seq(0, 45, by = 3),1)) +
  scale_y_continuous(limits = c(0,900))
# Distribution of order unit price
p3 <- ggplot(orders, aes(x=Order.Line.Unit.List.Price)) + 
  geom_density() +
  scale_x_continuous(name="Order Line Unit List Price",limits = c(-0.5,45.5),breaks = round(seq(0, 45, by = 3),1))
p4 <- ggplot(orders, aes(x=Order.Line.Unit.List.Price)) +
  geom_histogram(binwidth=1, colour="black", fill="white") +
  scale_x_continuous(name="Order Line Unit List Price",limits = c(-2.5,45.5),breaks = round(seq(0, 45, by = 3),1)) +
  scale_y_continuous(limits = c(0,900))
plotData <-tabyl(orders$Spend.Over..12.Per.Order.On.Average, sort = TRUE)
colnames(plotData) <- c("Spend.Over..12.Per.Order.On.Average","n","percent")
p5 <- ggplot(plotData, aes(x="", y=n, fill=Spend.Over..12.Per.Order.On.Average)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Customers that spend \nover 12$ on average") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))
plotData <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Day_of_week", "Over_12_dollar_percentage")
colnames(plotData) <- x
for (day in c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")){
  subset <- orders %>% filter(Order.Line.Day.of.Week == day)
  top <-tabyl(subset$Spend.Over..12.Per.Order.On.Average, sort = TRUE)
  colnames(top) <- c("Spend.Over..12.Per.Order.On.Average","n","percent")
  top <- top %>% filter(Spend.Over..12.Per.Order.On.Average == "True")
  percent <- top[1,"percent"]
  percent <- round(percent*100)
  print(top)
  print(percent)
  plotData[nrow(plotData) + 1,] = list(day,percent)
}
p6 <- ggplot(plotData, aes(x=reorder(Day_of_week,-Over_12_dollar_percentage),y=Over_12_dollar_percentage)) +
  geom_bar(stat="identity") +
  scale_x_discrete(name="Day of week") +
  scale_y_continuous(name="% of customers spending \nover 12$ per order\n on average")
multiplot(p1, p3, p5, p2, p4, p6, cols=2)

ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Price.png",sep=""),multiplot(p1, p3, p5, p2, p4, p6, cols=2), width=15)

# Distribution of order day of week and hour of day
p1 <- ggplot(orders,aes(x=reorder(Order.Line.Day.of.Week,Order.Line.Day.of.Week,function(x)-length(x)))) +
  geom_bar() +
  scale_x_discrete(name="Order Line Day of Week")
p2 <- ggplot(orders, aes(x=Order.Line.Hour.of.Day)) +
  geom_density() +
  scale_x_continuous(name="Order Line Hour of Day")
multiplot(p1, p2, cols=2)
ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Time.png",sep=""),multiplot(p1, p2, cols=2), width=15)

# Distribution of discounts
p1 <- ggplot(orders, aes(x=Order.Discount.Amount)) +
  geom_density() +
  scale_x_continuous(name="Order Discount Amount")
# Create ranking for top 10 promotion codes
top <-tabyl(orders$Order.Promotion.Code, sort = TRUE)
top <- top[with(top, order(top$n,decreasing=TRUE)),]
top <- head(top,10)
top <- top %>% select(1:2)
colnames(top) <- c("Order.Promotion.Code","n")
sumTotal <- nrow(orders)
sumTop <- sum(top$n)
countOthers <- sumTotal - sumTop
top[,c(1)] <- as.character(top[,c(1)])
top <- subset(top, !is.na(Order.Promotion.Code))
levels(top) <- c(levels(top),"(Others)")
top[nrow(top) + 1,] = list("(Others)",countOthers)
top[,c(1)] <- as.factor(top[,c(1)])
print(top)
p2 <- ggplot(top, aes(x=reorder(Order.Promotion.Code,-n),y=n)) +
  geom_bar(stat="identity") +
  scale_x_discrete(name="Order Promotion Code") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
multiplot(p1, p2, cols=2)
ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Discounts.png",sep=""),multiplot(p1, p2, cols=2), width=15)
