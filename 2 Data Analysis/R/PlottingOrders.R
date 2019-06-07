library(gridExtra)
library(dplyr)
library(janitor)
library(maps)
library(ggplot2)
library(tidyr)
library(plyr)
library(ggrepel) #for advanced labeling possibilities
source("./2 Data Analysis/R/PlottingUtilityFunctions.R")



useSmallVersions <- FALSE

# decide whether to use dev data or not
pathOrders <- "0 Data/order_data_cleaned_R.csv"
if (useSmallVersions){
  pathOrders <- "0 Data/order_data_small_R.csv"
}

# target path
pathPlotFolder <- "./4 Data Overview/Plots/"
pathTableFolder <- "./4 Data Overview/Tables/"

#read mapping table for US states plotting
usStates <- read.csv(file="2 Data Analysis/states.csv")

# read data
orders <- read.csv(file=pathOrders)

#color pallett
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#246d18")

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
# overview tables for interesting columns
#-----------------------------------------------------------------------------------
# order data
interestingOrders <- c("Order.Line.Quantity",
                        "Order.Line.Unit.List.Price",
                        "Order.Line.Amount",
                        "Spend.Over..12.Per.Order.On.Average",
                        "Order.Line.Day.of.Week",
                        "Order.Line.Hour.of.Day",
                        "Order.Promotion.Code",
                        "Order.Discount.Amount"
)
subset <- subset(orders, select=interestingOrders)
facCol <- summarizeFactorColumns(subset)
numCol <- summarizeNumericalColumns(subset)
write.table(facCol, file = paste(pathTableFolder,"OrderData_Factors.csv"), sep=",", row.names=FALSE)
write.table(numCol, file = paste(pathTableFolder,"OrderData_Numerical.csv"), sep=",", row.names=FALSE)
#-----------------------------------------------------------------------------------
# payment method
interestingPayments <- c("Order.Credit.Card.Brand",
                        "Bank.Card.Holder",
                        "Gas.Card.Holder",
                        "Upscale.Card.Holder",
                        "Unknown.Card.Type",
                        "TE.Card.Holder",
                        "Premium.Card.Holder",
                        "New.Bank.Card"
)
subset <- subset(orders, select=interestingPayments)
facCol <- summarizeFactorColumns(subset)
write.table(facCol, file = paste(pathTableFolder,"PaymentMethodData_Factors.csv"), sep=",", row.names=FALSE)
#-----------------------------------------------------------------------------------
# product data
interestingProducts <- c("StockType",
                        "Manufacturer",
                        "BrandName"
)
subset <- subset(orders, select=interestingProducts)
facCol <- summarizeFactorColumns(subset)
write.table(facCol, file = paste(pathTableFolder,"ProductData_Factors.csv"), sep=",", row.names=FALSE)
#-----------------------------------------------------------------------------------
# customer data
interestingCustomers <- c("City",
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
subset <- subset(orders, select=interestingCustomers)
facCol <- summarizeFactorColumns(subset)
numCol <- summarizeNumericalColumns(subset)
write.table(facCol, file = paste(pathTableFolder,"CustomerData_Factors.csv"), sep=",", row.names=FALSE)
write.table(numCol, file = paste(pathTableFolder,"CustomerData_Numerical.csv"), sep=",", row.names=FALSE)

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Plotting Order Data
#-----------------------------------------------------------------------------------
# Distribution of order amount, unit price, order quantity
p1 <- ggplot(orders, aes(x=Order.Line.Amount)) + 
      geom_density() +
      scale_x_continuous(name="Order Line Amount",
                         limits = c(-0.5,45.5),
                         breaks = round(seq(0, 45, by = 3),1)) +
      scale_y_continuous(limits = c(0,0.15)) +
      ggtitle("Distribution of the Order Amount")
p1 <- beautify(p1)

p2 <- ggplot(orders, aes(x=Order.Line.Unit.List.Price)) + 
      geom_density() +
      scale_x_continuous(name="Order Line Unit List Price",
                         limits = c(-0.5,45.5),
                         breaks = round(seq(0, 45, by = 3),1)) +
      ggtitle("Distrubution of the Unit List Price")
p2 <- beautify(p2)

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
p3 <- ggplot(data=plotData, aes(x=Date, y=Over_12_dollar_percentage, group=1)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = c(head(plotData,1)$Date, tail(plotData,1)$Date)) +
  scale_y_continuous(name="Orders over $12 in %")+
  ggtitle("History of Expenses over $ 12 in Percent")
p3 <- beautify(p3)

p4 <- meanOverTime(orders,"Order.Line.Date","Order.Line.Amount","Ø Amount in $") +
      theme_classic() +
      ggtitle("History of Average Order Amount")
p4 <- beautify(p4)

p5 <- meanOverTime(orders,"Order.Line.Date","Order.Line.Unit.List.Price","Ø Price in $") +
      theme_classic() +
      ggtitle("History of Average Unit List Price")
p5 <- beautify(p5)

p6 <- meanOverTime(orders,"Order.Line.Date","Order.Line.Quantity","Ø Quantity") +
      theme_classic() +
      ggtitle("History of Average Order Quantity")
p6 <- beautify(p6)

ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Price.png",sep=""),multiplot(p1, p2, p3, p4, p5, p6, cols=2), width=15, height=12)
#-----------------------------------------------------------------------------------
# Distribution of hour of day
beautify(ggplot(orders, aes(x=Order.Line.Hour.of.Day)) +
  geom_density() +
  scale_x_continuous(name="Hour of Day")) +
  ggtitle("Density of Orders over Hour of Day")
ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Time.png",sep=""), width=10)
#-----------------------------------------------------------------------------------
# Order Discounts: Distribution of discounts
beautify(ggplot(orders, aes(x=Order.Discount.Amount)) +
  geom_density() +
  scale_x_continuous(name="Discount Amount") +
  ggtitle("Density of Order Amount"))
ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Discounts.png",sep=""), width=10)
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Plotting product data
#-----------------------------------------------------------------------------------
#Manufacturer lorenz curve
plotBarAndLorenz(orders, "Manufacturer") +
        ggtitle("Lorenz Curve Manufacturers")
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/LorenzManufacturer.png",sep=""), width=12, height=13)
#-----------------------------------------------------------------------------------
#Product ID lorenz curve
plotBarAndLorenz(orders, "Order.Line.Subassortment.ID") +
  ggtitle("Lorenz Curve Products")
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/LorenzProducts.png",sep=""), width=20, height=7)
#-----------------------------------------------------------------------------------
#BrandName lorenz curve
plotBarAndLorenz(orders, "BrandName") +
  ggtitle("Lorenz Curve Brands")
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/LorenzBrandName.png",sep=""), width=12, height=13)
#-----------------------------------------------------------------------------------
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
  scale_fill_grey() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=centralSize)) +
  ggtitle("Stock Type per Brand")
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
  scale_fill_grey() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=centralSize)) +
  ggtitle("Stock Type per Manufacurer")
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/StockPer.png",sep=""),multiplot(p1, p2, cols=2), width=15)
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Plotting payment method
#-----------------------------------------------------------------------------------
#CardBrand per
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
p1 <- beautify(ggplot(top, aes(fill=Premium, y=amount, x=CardBrand)) + 
  geom_bar( stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_grey() +
  ggtitle("Ratio of Premium Cards per Brand"))
#Upscale Card per Card Brand
top <- orders
top <- table(top$Upscale.Card.Holder, top$Order.Credit.Card.Brand, useNA="always")
top <- top[, which(colSums(top) != 0)] #drop columns not included in top 10
top <- top[, order(colSums(top), decreasing=TRUE)]
colnames(top)[is.na(colnames(top))] <- 'Others'
rownames(top)[is.na(rownames(top))] <- 'No value'
top <- data.frame(top)
names(top) = c("Upscale", "CardBrand", "amount")
print(top)
# Stacked Plot
p2 <- beautify(ggplot(top, aes(fill=Upscale, y=amount, x=CardBrand)) + 
  geom_bar( stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_grey()+
  ggtitle("Ratio of Upscale Cards per Brand"))
ggsave(filename=paste(pathPlotFolder,"Payment Method Data/CardBrand.png",sep=""),multiplot(p1, p2, cols=2), width=15)
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Plotting Customer Data
#-----------------------------------------------------------------------------------
# US States heatmap
states <- usStates
states$State <- tolower(states$State)
gusa <- map_data("state")
plotData <- merge(orders, states, by.x="US.State", group = group, by.y="Abbreviation")
plotData <-tabyl(plotData$State, sort = TRUE)
names(plotData) <- c("region", "count", "percentage")
print(plotData)
top <- head(arrange(plotData, desc(count)), 10)
top <- top$region
centroids <- summarize(group_by(gusa, region), x = mean(range(long)), y = mean(range(lat))) #Calculate centroids
names(centroids)[1] <- "state"
centroids <- centroids[centroids$state %in% top,]
plotData <- left_join(gusa, plotData)
plotData[is.na(plotData)] <- 0
ggplot(data = plotData, mapping = aes(x = long, y = lat)) +
  geom_polygon(aes(group=group, fill=count), color = "black", size = 0.3) +
  labs(x = NULL, y = NULL, fill = "Count", title = "Customer Hotspots") +
  scale_fill_gradient(low = "white", high = "black") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank()) +
  #geom_label_repel(data = centroids, aes(x, y, label=state), force=2, segment.color="#fd9409", nudge_x=0.5) +
  coord_map()
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/States.png",sep=""), bg = "transparent", width=13)
#-----------------------------------------------------------------------------------
#most important cities (map)
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
  geom_point(cities, mapping = aes(x = long, y = lat), size=(cities$count)/5, color="black", alpha = .15) +
  geom_text(data=cities,aes(x=long, y=lat, label=name), color = "black", check_overlap = TRUE, size = 3) +
  labs(title = "Top 100 Customer Cities") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank())
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/Cities.png",sep=""), width=13)
#-----------------------------------------------------------------------------------
#age data
p1 <- beautify(ggplot(orders, aes(x=Age)) + 
  geom_density() +
  scale_x_continuous(name="Age")) +
  labs(title = "Age Distribution")

plotData <- na.omit(orders %>% select(Age, Gender))
p2 <-beautify(ggplot(plotData) + 
  stat_density(aes(x=Age, linetype=Gender), geom="line", position="identity") +
  labs(title = "Age Distribution per Gender") +
  scale_linetype_discrete(guide=guide_legend(override.aes=aes(colour="black"))))
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/Age.png",sep=""), multiplot(p1,p2, cols=2), width=16, height=5)