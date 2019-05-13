library(gridExtra)
library(dplyr)
library(janitor)
library(maps)
library(ggplot2)
library(ggrepel)

useSmallVersions <- FALSE

# decide whether to use dev data or not
pathOrders <- "0 Data/order_data_cleaned_R.csv"
pathClicks  <- "0 Data/clickstream_data_cleaned_R.csv"
if (useSmallVersions){
  pathOrders <- "0 Data/order_data_small_R.csv"
  pathClicks <- "0 Data/clickstream_data_small_R.csv"
}

# target path
pathPlotFolder <- "./4 Data Overview/Plots/"
pathTableFolder <- "./4 Data Overview/Tables/"

# read data
orders <- read.csv(file=pathOrders)
clicks <- read.csv(file=pathClicks)
usStates <- read.csv(file="2 Data Analysis/states.csv")
#-----------------------------------------------------------------------------------
# overview table of order data
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
colnames(summary) <- c("Order Line Quantity",
                        "Order Line Unit List Price",
                        "Order Line Amount",
                        "Spend Over $12 Per Order On Average",
                        "Order Line Day of Week",
                        "Order Line Hour of.Day",
                        "Order Promotion Code",
                        "Order Discount Amount"
                       )
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
colnames(summary) <- c("Order Credit Card.Brand",
                       "Bank Card Holder",
                       "Gas Card Holder",
                       "Upscale Card Holder",
                       "Unknown Card Type",
                       "TE Card Holder",
                       "Premium Card Holder",
                       "New Bank Card"
                       )
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
colnames(summary) <- c("City",
                       "Country",
                       "US State",
                       "Age",
                       "Marital Status",
                       "Gender",
                       "Audience",
                       "Truck Owner",
                       "RV Owner",
                       "Motorcycle Owner",
                       "Working Woman",
                       "Presence Of Children",
                       "Speciality Store Retail",
                       "Oil Retail Activity",
                       "Bank Retail Activity",
                       "Finance Retail Activity",
                       "Miscellaneous Retail Activity",
                       "Upscale Retail",
                       "Upscale Speciality Retail",
                       "Retail Activity"
)
write.table(summary, file = paste(pathTableFolder,"CustomerData.csv"), sep=",", row.names=FALSE)

#-----------------------------------------------------------------------------------
# Overview table of clickstream data
interestingColumns <- c("BrandName",
                        "UnitsPerInnerBox",
                        "PrimaryPackage",
                        "Depth",
                        "VendorMinREOrderDollars",
                        "Cat1Sub2",
                        "Height",
                        "UnitsPerOuterBox",
                        "StockType",
                        "Pack",
                        "ProductForm",
                        "Look",
                        "BasicOrFashion",
                        "SaleOrNonSale",
                        "Length",
                        "Cat1Sub3",
                        "ColorOrScentDropdown",
                        "DisContinuedInd",
                        "Socktype2",
                        "MinQty",
                        "LeadTime",
                        "Terms",
                        "Weight",
                        "Socktype1",
                        "InOrOutofStock",
                        "HasDressingRoom",
                        "ColorOrScent",
                        "Width",
                        "Texture",
                        "WeightUOM",
                        "Manufacturer",
                        "ExplodedProdKits",
                        "DimUOM",
                        "ToeFeature",
                        "Category2",
                        "Material",
                        "CategoryCode",
                        "Cat1Sub1",
                        "UnitIncrement",
                        "WaistControl",
                        "Collection",
                        "BodyFeature",
                        "Audience",
                        "Category1",
                        "Freight",
                        "Product",
                        "Cat2Sub1",
                        "ActionCode",
                        "Pattern"
)
summary <- summary(clicks[interestingColumns])
summary[is.na(summary)]<-""
write.table(summary, file = paste(pathTableFolder,"ClickstreamData.csv"), sep=",", row.names=FALSE)

#-----------------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------------
#Plots for order data

# Distribution of order amount, unit price, order quantity
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
  labs(x = NULL, y = NULL, fill = NULL, title = "Customers that spend \nover $12 on average") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))
# plotData <- data.frame(matrix(ncol = 2, nrow = 0))
# x <- c("Day_of_week", "Over_12_dollar_percentage")
# colnames(plotData) <- x
# for (day in c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")){
#   subset <- orders %>% filter(Order.Line.Day.of.Week == day)
#   top <-tabyl(subset$Spend.Over..12.Per.Order.On.Average, sort = TRUE)
#   colnames(top) <- c("Spend.Over..12.Per.Order.On.Average","n","percent")
#   top <- top %>% filter(Spend.Over..12.Per.Order.On.Average == "True")
#   percent <- top[1,"percent"]
#   percent <- round(percent*100)
#   print(top)
#   print(percent)
#   plotData[nrow(plotData) + 1,] = list(day,percent)
# }
# p6 <- ggplot(plotData, aes(x=reorder(Day_of_week,-Over_12_dollar_percentage),y=Over_12_dollar_percentage)) +
#   geom_bar(stat="identity") +
#   scale_x_discrete(name="Day of week") +
#   scale_y_continuous(name="% of customers spending \nover $12 per order\n on average")
plotData <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Date", "Over_12_dollar_percentage")
colnames(plotData) <- x
for (day in orders$Order.Line.Date){
  if (!substring(day,6) %in% plotData$Date){
    subset <- orders %>% filter(Order.Line.Date == day)
    subsetTrue = subset %>% filter(Spend.Over..12.Per.Order.On.Average == "True")
    percentage = round(nrow(subsetTrue)/nrow(subset),2)
    plotData[nrow(plotData) + 1,] = list(substring(day,6),percentage)
  }
}
plotData <- plotData %>% arrange(Date)
p6 <- ggplot(data=plotData, aes(x=Date, y=Over_12_dollar_percentage, group=1)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = c(head(plotData,1)$Date, tail(plotData,1)$Date)) +
  scale_y_continuous(name="Percentage of customers \nthat spend over $12\n on average")

p7 <- ggplot(orders, aes(x=Order.Line.Quantity)) +
  geom_histogram(binwidth=1, colour="black", fill="white") +  # Overlay with transparent density plot
  scale_x_continuous(name ="Order Line Quantity",limits = c(0.5,7.5),breaks = round(seq(1, 7, by = 1),1))

plotData <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Date", "Avg_Order_Quantity")
colnames(plotData) <- x
for (day in orders$Order.Line.Date){
  if (!substring(day,6) %in% plotData$Date){
    subset <- orders %>% filter(Order.Line.Date == day)
    quantity = mean(subset$Order.Line.Quantity)
    plotData[nrow(plotData) + 1,] = list(substring(day,6),quantity)
  }
}
plotData <- plotData %>% arrange(Date)
p8 <- ggplot(data=plotData, aes(x=Date, y=Avg_Order_Quantity, group=1)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = c(head(plotData,1)$Date, tail(plotData,1)$Date)) +
  scale_y_continuous(name="Average order \nquantity")

multiplot(p1, p3, p5, p7, p2, p4, p6, p8, cols=2)

ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Price.png",sep=""),multiplot(p1, p3, p5, p7, p2, p4, p6, p8, cols=2), width=15, height=10)

# Distribution of order day of week and hour of day
p1 <- ggplot(orders,aes(x=reorder(Order.Line.Day.of.Week,Order.Line.Day.of.Week,function(x)-length(x)))) +
  geom_bar() +
  scale_x_discrete(name="Order Line Day of Week")
p2 <- ggplot(orders, aes(x=Order.Line.Hour.of.Day)) +
  geom_density() +
  scale_x_continuous(name="Order Line Hour of Day")
multiplot(p1, p2, cols=2)
ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Time.png",sep=""),multiplot(p1, p2, cols=2), width=15)

# Order Discounts: Distribution of discounts
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

#-----------------------------------------------------------------------------------
#Plots product data
#Create ranking for Stock Type
top <- (tabyl(orders$StockType)) %>% select(1:2) #create frequency table
names(top) <- c("StockType", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
top[is.na(top)] <- "Others"
top <- arrange(top, desc(amount))
top$StockType <- factor(top$StockType, levels = top$StockType) #lockOrder
print(top)
ggplot(top, aes(StockType, amount), y=amount) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/StockType.png",sep=""))


# Create ranking for Manufacturer
firstx = 10
top <- (tabyl(orders$Manufacturer)) %>% select(1:2) #create frequency table
names(top) <- c("Manufacturer", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
top[is.na(top)] <- "Others"
top <- arrange(top, desc(amount))
top <- head(top, firstx) #only first 10 rows3
top$Manufacturer <- factor(top$Manufacturer, levels = top$Manufacturer) #lockOrder
print(top)
ggplot(top, aes(Manufacturer, amount), y=amount) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/ManufacturerTop10.png",sep=""))


# Create ranking for Manufacturer
firstx = 50
top <- (tabyl(orders$Order.Line.Subassortment.ID)) %>% select(1,3) #create frequency table
names(top) <- c("Order.Line.Subassortment.ID", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
top[is.na(top)] <- "Others"
top <- arrange(top, desc(amount))
amountTotal <- sum(top$amount)
amountTop <- sum(head(top, firstx)$amount)
top <- head(top, firstx) #only first 25 rows
top[nrow(top) + 1,] = list("Others",amountTotal-amountTop)
top <- top %>% arrange(desc(amount))
#top$Order.Line.Subassortment.ID <- factor(top$Order.Line.Subassortment.ID, levels = top$Order.Line.Subassortment.ID) #lockOrder
print(top)
ggplot(top, aes(reorder(Order.Line.Subassortment.ID,-amount),amount), y=amount) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(name="Product ID") +
  scale_y_continuous(name="Order Quantity Percentage") +
  ggtitle("Order Quantity Share of the Top 50 products") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/ProductsTop25.png",sep=""), width = 15, height = 7)



# Create ranking for BrandName
firstx = 10
top <- (tabyl(orders$BrandName)) %>% select(1:2) #create frequency table
names(top) <- c("BrandName", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
top <- arrange(top, desc(amount))
top <- head(top, firstx) #only first 10 rows3
topBrands <- top$BrandName
top[is.na(top)] <- "Others"
top$BrandName <- factor(top$BrandName, levels = top$BrandName) #lockOrder
print(top)
ggplot(top, aes(BrandName, amount), y=amount) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/BrandNameTop10.png",sep=""))

#StockPerBrand
top <- orders
top <- table(top$StockType, top$BrandName, useNA="always")
top <- top[, which(colSums(top) != 0)] #drop columns not included in top 10
top <- top[, order(colSums(top), decreasing=TRUE)]
colnames(top)[is.na(colnames(top))] <- 'Others'
rownames(top)[is.na(rownames(top))] <- 'Others'
top <- data.frame(top)
names(top) = c("StockType", "BrandName", "amount")
print(top)
# Stacked Plot
p1 <- ggplot(top, aes(fill=StockType, y=amount, x=BrandName)) + 
  geom_bar( stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#StockPerManufacturer
top <- orders
top <- table(top$StockType, top$Manufacturer, useNA="always")
top <- top[, which(colSums(top) != 0)] #drop columns not included in top 10
top <- top[, order(colSums(top), decreasing=TRUE)]
colnames(top)[is.na(colnames(top))] <- 'Others'
rownames(top)[is.na(rownames(top))] <- 'Others'
top <- data.frame(top)
names(top) = c("StockType", "Manufacturer", "amount")
print(top)
# Stacked Plot
p2 <- ggplot(top, aes(fill=StockType, y=amount, x=Manufacturer)) + 
  geom_bar( stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/StockPer.png",sep=""),multiplot(p1, p2, cols=2), width=15)

#------------------------------------------------------------------------
#Plots for Payment Method
#Cards
plotData <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("CardType", "percentage_yes")
colnames(plotData) <- x
columnList <- c("Bank.Card.Holder", "Gas.Card.Holder", "Upscale.Card.Holder", "Unknown.Card.Type", "TE.Card.Holder", "Premium.Card.Holder", "New.Bank.Card")
labelList <- c("Bank Card", "Gas Card", "Upscale Card", "Unknown Card", "TE Card", "Premium Card", "New Bank Card")
i <- 1
for (sector in columnList){
  top <- tabyl(orders[,sector], sort = TRUE, show_na = FALSE)
  colnames(top) <- c(sector,"n","percent")
  percentage <- (round(top[2,"percent"],2))*100
  plotData[nrow(plotData) + 1,] = list(labelList[[i]],percentage)
  i <- i+1
}
plotData <- plotData %>% arrange(desc(percentage_yes))
ggplot(plotData, aes(x=reorder(CardType,-percentage_yes),y=percentage_yes)) +
  geom_bar(stat="identity") +
  scale_x_discrete(name="Card Type") +
  scale_y_continuous(name="Percentage of Holders") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename=paste(pathPlotFolder,"Payment Method Data/CardTypes.png",sep=""), width=10)

#Card Holder
plotData <-tabyl(orders$Bank.Card.Holder, sort = TRUE, show_na = FALSE)
colnames(plotData) <- c("CardHolder","n","percent")
plotData[,c(1)] <- as.character(plotData[,c(1)])
plotData[is.na(plotData)] <- "No value"
plotData$CardHolder[plotData$CardHolder == "True"] <- "Yes"
plotData$CardHolder[plotData$CardHolder == "False"] <- "No"
print(plotData)
p1 <- ggplot(plotData, aes(x="", y=n, fill=CardHolder)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Card Holder") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

#Gas Card
plotData <- tabyl(orders$Gas.Card.Holder, sort = TRUE, show_na = FALSE)
colnames(plotData) <- c("GasCard","n","percent")
plotData[,c(1)] <- as.character(plotData[,c(1)])
plotData[is.na(plotData)] <- "No value"
plotData$GasCard[plotData$GasCard == "True"] <- "Yes"
plotData$GasCard[plotData$GasCard == "False"] <- "No"
print(plotData)
p2 <- ggplot(plotData, aes(x="", y=n, fill=GasCard)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Gas Card") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

plotData <- tabyl(orders$ Upscale.Card.Holder, sort = TRUE, show_na = FALSE)
print(plotData)
colnames(plotData) <- c("UpscaleCard","n","percent")
plotData[,c(1)] <- as.character(plotData[,c(1)])
plotData[is.na(plotData)] <- "No value"
plotData$UpscaleCard[plotData$UpscaleCard == "True"] <- "Yes"
plotData$UpscaleCard[plotData$UpscaleCard == "False"] <- "No"
print(plotData)
p3 <- ggplot(plotData, aes(x="", y=n, fill=UpscaleCard)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Upscale Card") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

plotData <- tabyl(orders$Unknown.Card.Type, sort = TRUE, show_na = FALSE)
print(plotData)
colnames(plotData) <- c("UnknownCard","n","percent")
plotData[,c(1)] <- as.character(plotData[,c(1)])
plotData[is.na(plotData)] <- "No value"
plotData$UnknownCard[plotData$UnknownCard == "True"] <- "Yes"
plotData$UnknownCard[plotData$UnknownCard == "False"] <- "No"
print(plotData)
p4 <- ggplot(plotData, aes(x="", y=n, fill=UnknownCard)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Unknow Card") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

plotData <- tabyl(orders$TE.Card.Holder, sort = TRUE, show_na = FALSE)
print(plotData)
colnames(plotData) <- c("TECard","n","percent")
plotData[,c(1)] <- as.character(plotData[,c(1)])
plotData[is.na(plotData)] <- "No value"
plotData$TECard[plotData$TECard == "True"] <- "Yes"
plotData$TECard[plotData$TECard == "False"] <- "No"
print(plotData)
p5 <- ggplot(plotData, aes(x="", y=n, fill=TECard)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "TE Card") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

plotData <- tabyl(orders$Premium.Card.Holder, sort = TRUE, show_na = FALSE)
print(plotData)
colnames(plotData) <- c("PremiumCard","n","percent")
plotData[,c(1)] <- as.character(plotData[,c(1)])
plotData[is.na(plotData)] <- "No value"
plotData$PremiumCard[plotData$PremiumCard == "True"] <- "Yes"
plotData$PremiumCard[plotData$PremiumCard == "False"] <- "No"
print(plotData)
p6 <- ggplot(plotData, aes(x="", y=n, fill=PremiumCard)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Premium Card") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

plotData <- tabyl(orders$New.Bank.Card, sort = TRUE, show_na = FALSE)
print(plotData)
colnames(plotData) <- c("NewBank","n","percent")
plotData[,c(1)] <- as.character(plotData[,c(1)])
plotData[is.na(plotData)] <- "No value"
plotData$NewBank[plotData$NewBank == "True"] <- "Yes"
plotData$NewBank[plotData$NewBank == "False"] <- "No"
print(plotData)
p7 <- ggplot(plotData, aes(x="", y=n, fill=NewBank)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100, digits = 2),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "New Card") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

ggsave(filename=paste(pathPlotFolder,"Payment Method Data/CardInfos.png",sep=""),multiplot(p1, p2, p3, p4, p5, p6, p7, cols=4), width=15)

#CardTypes
top <- orders
top <- table(top$Premium.Card.Holder, top$Order.Credit.Card.Brand, useNA="always")
top <- top[, which(colSums(top) != 0)] #drop columns not included in top 10
top <- top[, order(colSums(top), decreasing=TRUE)]
colnames(top)[is.na(colnames(top))] <- 'Others'
rownames(top)[is.na(rownames(top))] <- 'No value'
top <- data.frame(top)
names(top) = c("Premium", "CardBrand", "amount")
print(top)
# Stacked Plot
p1 <- ggplot(top, aes(fill=Premium, y=amount, x=CardBrand)) + 
  geom_bar( stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#StockPerManufacturer
top <- orders
top <- table(top$New.Bank.Card, top$Order.Credit.Card.Brand, useNA="always")
top <- top[, which(colSums(top) != 0)] #drop columns not included in top 10
top <- top[, order(colSums(top), decreasing=TRUE)]
colnames(top)[is.na(colnames(top))] <- 'Others'
rownames(top)[is.na(rownames(top))] <- 'No value'
top <- data.frame(top)
names(top) = c("New", "CardBrand", "amount")
print(top)
# Stacked Plot
p2 <- ggplot(top, aes(fill=New, y=amount, x=CardBrand)) + 
  geom_bar( stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename=paste(pathPlotFolder,"Payment Method Data/CardBrand.png",sep=""),multiplot(p1, p2, cols=2), width=15)

#------------------------------------------------------------------------
#Customer Data
states <- usStates
states$State <- tolower(states$State)
gusa <- map_data("state")
plotData <- merge(orders, states, by.x="US.State", group = group, by.y="Abbreviation")
plotData <-tabyl(plotData$State, sort = TRUE)
names(plotData) <- c("region", "count", "percentage")
print(plotData)
top <- head(arrange(plotData, desc(count)), 10)
top <- top$region
#Calculate centroids
centroids <- summarize(group_by(gusa, region), x = mean(range(long)), y = mean(range(lat)))
names(centroids)[1] <- "state"
centroids <- centroids[centroids$state %in% top,]
plotData <- left_join(gusa, plotData)
plotData[is.na(plotData)] <- 0
ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
  geom_polygon(aes(group=group, fill=count), color = "#fd9409", size = 0.3) +
  labs(x = NULL, y = NULL, fill = "Count", title = "Customer Hotspots") +
  scale_fill_gradient(low = "white", high = "black") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank()) +
  geom_label_repel(data = centroids, aes(x, y, label=state), force=2, segment.color="#fd9409", nudge_x=0.5) +
  coord_map()
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/States.png",sep=""), bg = "transparent", width=13)

#cities
states <- usStates
plotData <- merge(orders, states, by.x="US.State", by.y="Abbreviation")
plotData$name<- with(plotData, paste0(City, " ", US.State)) #new column with city and state
plotData <-tabyl(plotData$name, sort = TRUE)
names(plotData) <- c("name", "count", "percentage")
plotData <- left_join(us.cities, plotData)
plotData <- na.omit(plotData) #drop cities without orders
plotData <- arrange(plotData, desc(count))
cities <- head(plotData, 100)
print(cities)
ggplot() + 
  geom_map(data=map_data("state"), map=map_data("state"), aes(map_id=region),fill="grey", color="white", size=0.15) +
  geom_point(cities, mapping = aes(x = long, y = lat), size=(cities$count)/5, color="#fd9409", alpha = .4) +
  geom_text(data=cities,aes(x=long, y=lat, label=name), color = "black", check_overlap = TRUE, size = 3) +
  labs(title = "Top 100 Customer Cities") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank())
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/Cities.png",sep=""), width=13)

# retail activity
plotData <- tabyl(orders$Retail.Activity, sort = TRUE, show_na = FALSE)
print(plotData)
colnames(plotData) <- c("Retail.Activity","n","percent")
plotData[,c(1)] <- as.character(plotData[,c(1)])
plotData[is.na(plotData)] <- "No value"
plotData$Retail.Activity[plotData$Retail.Activity == "True"] <- "Yes"
plotData$Retail.Activity[plotData$Retail.Activity == "False"] <- "No"
print(plotData)
p1 <- ggplot(plotData, aes(x="", y=n, fill=Retail.Activity)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100, digits = 2),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Retail Activity") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

plotData <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Retail_Activity", "percentage_yes")
colnames(plotData) <- x
columnList <- c("Speciality.Store.Retail","Oil.Retail.Activity","Bank.Retail.Activity",
                "Finance.Retail.Activity","Miscellaneous.Retail.Activity","Upscale.Retail",
                "Upscale.Speciality.Retail")
labelList <- c("Speciality Store Retail","Oil Retail Activity","Bank Retail Activity",
               "Finance Retail Activity","Miscellaneous Retail Activity","Upscale Retail",
               "Upscale Speciality Retail")
i <- 1
for (sector in columnList){
  top <- tabyl(orders[,sector], sort = TRUE, show_na = FALSE)
  colnames(top) <- c(sector,"n","percent")
  percentage <- round(top[2,"percent"],2)*100
  plotData[nrow(plotData) + 1,] = list(labelList[[i]],percentage)
  i <- i+1
}
plotData <- plotData %>% arrange(desc(percentage_yes))
p2 <- ggplot(plotData, aes(x=reorder(Retail_Activity,-percentage_yes),y=percentage_yes)) +
  geom_bar(stat="identity") +
  scale_x_discrete(name="Retail Activity Type") +
  scale_y_continuous(name="Percentage of active customers") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

multiplot(p1,p2, cols=2)
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/Retail_Activity.png",sep=""),multiplot(p1,p2, cols=2), width=8, height=5)


#social data
plotData <- tabyl(orders$Gender, sort = TRUE, show_na = FALSE)
colnames(plotData) <- c("Gender","n","percent")
p1 <- ggplot(plotData, aes(x="", y=n, fill=Gender)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100, digits = 0),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Gender") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666")) +
  scale_fill_manual(values = c("Female" = "#55DDE0",
                               "Male" = "#F26419"))

plotData <- orders %>% filter(Gender == "Female")
plotData <- tabyl(plotData$Marital.Status, sort = TRUE, show_na = FALSE)
colnames(plotData) <- c("Marital.Status","n","percent")
p2 <- ggplot(plotData, aes(x="", y=n, fill=Marital.Status)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100, digits = 0),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Marital Status of\nFemale Customers") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

plotData <- orders %>% filter(Gender == "Male")
plotData <- tabyl(plotData$Marital.Status, sort = TRUE, show_na = FALSE)
colnames(plotData) <- c("Marital.Status","n","percent")
p6 <- ggplot(plotData, aes(x="", y=n, fill=Marital.Status)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100, digits = 0),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Marital Status of\nMale Customers") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

plotData <- orders %>% filter(Gender == "Female")
plotData <- tabyl(plotData$Working.Woman, sort = TRUE, show_na = FALSE)
colnames(plotData) <- c("Working.Woman","n","percent")
print(plotData)
p5 <- ggplot(plotData, aes(x="", y=n, fill=Working.Woman)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100, digits = 0),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Working Woman") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666")) +
  scale_fill_manual(values = c("True" = "#55DDE0",
                               "False" = "#F26419"))

plotData <- tabyl(orders$Presence.Of.Children, sort = TRUE, show_na = FALSE)
colnames(plotData) <- c("Presence.Of.Children","n","percent")
p4 <- ggplot(plotData, aes(x="", y=n, fill=Presence.Of.Children)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100, digits = 0),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Presence of Children") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

p3 <- ggplot(orders, aes(x=Age)) + 
  geom_density() +
  scale_x_continuous(name="Age")

plotData <- na.omit(orders %>% select(Age, Gender))
p7 <- ggplot(plotData) + 
  geom_density(aes(x=Age, colour=Gender),show_guide=FALSE)+
  stat_density(aes(x=Age, colour=Gender), geom="line",position="identity") +
  labs(title = "Age Distribution per Gender")

plotData <- tabyl(orders$Number.Of.Adults, sort = TRUE, show_na = FALSE)
colnames(plotData) <- c("Number.Of.Adults","n","percent")
p8 <- ggplot(plotData, aes(x="", y=n, fill=Number.Of.Adults)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste(round(percent*100, digits = 0),"%",sep="")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Number of Adults \n in Household") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

multiplot(p1,p2,p3,p4,p5,p6,p7,p8, cols=2)
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/Social_Data.png",sep=""),multiplot(p1,p2,p3,p4,p5,p6,p7,p8, cols=2), width=13, height=15)


#Vehicle Ownership
plotData <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Vehicle", "percentage_yes")
colnames(plotData) <- x
columnList <- c("Truck.Owner", "Motorcycle.Owner", "RV.Owner")
labelList <- c("Truck","Motorcycle","RV")
i <- 1
for (sector in columnList){
  top <- tabyl(orders[,sector], sort = TRUE, show_na = FALSE)
  colnames(top) <- c(sector,"n","percent")
  percentage <- round(top[2,"percent"],2)
  plotData[nrow(plotData) + 1,] = list(labelList[[i]],percentage)
  i <- i+1
}
plotData <- plotData %>% arrange(desc(percentage_yes))
ggplot(plotData, aes(x=reorder(Vehicle,-percentage_yes),y=percentage_yes)) +
  geom_bar(stat="identity") +
  scale_x_discrete(name="Vehicle") +
  scale_y_continuous(name="Percentage of owners") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/VehicleOwnership.png",sep=""), width=4)
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#Plots for clickstream data
# Distribution of brand names
ggplot(data=subset(clicks, !is.na(BrandName)), aes(BrandName)) +
  geom_bar(width=.5, fill="tomato3") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Brand Name Quantity.png",sep=""))
