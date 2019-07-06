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
#Plotting Order Process Data
#-----------------------------------------------------------------------------------
# Distribution of order amount, unit price, order quantity
p1 <- ggplot(orders, aes(x=Order.Line.Amount)) + 
      geom_density() +
      scale_x_continuous(name="Order Line Amount",
                         limits = c(-0.5,45.5),
                         breaks = round(seq(0, 45, by = 3),1)) +
      scale_y_continuous(limits = c(0,0.15)) +
      ggtitle("Orders: Distribution of the Order Amount")
p1 <- beautify(p1)

p2 <- ggplot(orders, aes(x=Order.Line.Unit.List.Price)) + 
      geom_density() +
      scale_x_continuous(name="Order Line Unit List Price",
                         limits = c(-0.5,45.5),
                         breaks = round(seq(0, 45, by = 3),1)) +
      ggtitle("Orders: Distrubution of the Unit List Price")
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
plotData$Over_12_dollar_percentage <- plotData$Over_12_dollar_percentage * 100
p3 <- ggplot(data=plotData, aes(x=Date, y=Over_12_dollar_percentage, group=1)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = c(head(plotData,1)$Date, tail(plotData,1)$Date)) +
  scale_y_continuous(name="Orders over $12 in %")+
  ggtitle("Orders: History of Expenses over $ 12 in Percent")
p3 <- beautify(p3)

p4 <- meanOverTime(orders,"Order.Line.Date","Order.Line.Amount","Ø Amount in $") +
      theme_classic() +
      ggtitle("Orders: History of Average Order Amount")
p4 <- beautify(p4)

p5 <- meanOverTime(orders,"Order.Line.Date","Order.Line.Unit.List.Price","Ø Price in $") +
      theme_classic() +
      ggtitle("Orders: History of Average Unit List Price")
p5 <- beautify(p5)

p6 <- meanOverTime(orders,"Order.Line.Date","Order.Line.Quantity","Ø Quantity") +
      theme_classic() +
      ggtitle("Orders: History of Average Order Quantity")
p6 <- beautify(p6)

ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Price.png",sep=""),multiplot(p1, p2, p3, p4, p5, p6, cols=2), width=15, height=12)
#-----------------------------------------------------------------------------------
# Distribution of hour of day
beautify(ggplot(orders, aes(x=Order.Line.Hour.of.Day)) +
  geom_density() +
  scale_x_continuous(name="Hour of Day")) +
  ggtitle("Orders: Density of Orders over Hour of Day")
ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Time.png",sep=""), width=10)
#-----------------------------------------------------------------------------------
# Order Discounts: Distribution of discounts
beautify(ggplot(orders, aes(x=Order.Discount.Amount)) +
  geom_density() +
  scale_x_continuous(name="Discount Amount") +
  ggtitle("Orders: Density of Discount"))
ggsave(filename=paste(pathPlotFolder,"Order Data Plots/Order Discounts.png",sep=""), width=10)
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Plotting product data
#-----------------------------------------------------------------------------------
#Manufacturer lorenz curve
beautify(plotLorenzCurve(orders, "Manufacturer") +
           ggtitle("Orders: Lorenz Curve Manufacturers")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/LorenzManufacturer.png",sep=""), width=5, height=5)
#-----------------------------------------------------------------------------------
#Product ID lorenz curve
beautify(plotLorenzCurve(orders, "Order.Line.Subassortment.ID") +
           ggtitle("Orders: Lorenz Curve Products")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/LorenzProducts.png",sep=""), width=5, height=5)
#-----------------------------------------------------------------------------------
#BrandName lorenz curve
beautify(plotLorenzCurve(orders, "BrandName") +
           ggtitle("Orders: Lorenz Curve Brands")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
ggsave(filename=paste(pathPlotFolder,"Product Data Plots/LorenzBrandName.png",sep=""), width=5, height=5)
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
  ggtitle("Orders: Stock Type per Brand")
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
  ggtitle("Orders: Stock Type per Manufacurer")
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
  ggtitle("Orders: Ratio of Premium Cards per Brand"))
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
  ggtitle("Orders: Ratio of Upscale Cards per Brand"))
ggsave(filename=paste(pathPlotFolder,"Payment Method Data/CardBrand.png",sep=""),multiplot(p1, p2, cols=2), width=15)
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Plotting Customer Data
#-----------------------------------------------------------------------------------
# US States heatmap (weighted)
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
  labs(x = NULL, y = NULL, fill = "Count", title = "Orders: Customer Hotspots (weighted by order amount)") +
  scale_fill_gradient(low = "white", high = "black") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank()) +
  #geom_label_repel(data = centroids, aes(x, y, label=state), force=2, segment.color="#fd9409", nudge_x=0.5) +
  coord_map()
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/States_weighted.png",sep=""), bg = "transparent", width=13)

# US States heatmap
states <- usStates
states$State <- tolower(states$State)
gusa <- map_data("state")
ordersDistinctCustomer <- distinct(orders, Customer.ID, .keep_all = TRUE)
plotData <- merge(ordersDistinctCustomer, states, by.x="US.State", group = group, by.y="Abbreviation")
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
  labs(x = NULL, y = NULL, fill = "Count", title = "Orders: Customer Hotspots") +
  scale_fill_gradient(low = "white", high = "black") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank()) +
  #geom_label_repel(data = centroids, aes(x, y, label=state), force=2, segment.color="#fd9409", nudge_x=0.5) +
  coord_map()
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/States.png",sep=""), bg = "transparent", width=13)
#-----------------------------------------------------------------------------------
#most important cities (map) (weighted)
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
  geom_point(cities, mapping = aes(x = long, y = lat), size=(cities$count)/5, color="orange", alpha = .5) +
  geom_text(data=cities,aes(x=long, y=lat, label=name), color = "black", check_overlap = TRUE, size = 3) +
  labs(title = "Orders: Top 100 Customer Cities (weighted by order amount)") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank())
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/Cities_weighted.png",sep=""), width=13)

#most important cities (map)
states <- usStates %>% filter(State != "Alaska") #TODO: Possible bug? Why isn't this bugging the weighted version??
plotData <- merge(ordersDistinctCustomer, states, by.x="US.State", by.y="Abbreviation")
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
  geom_point(cities, mapping = aes(x = long, y = lat), size=(cities$count)/5, color="orange", alpha = .5) +
  geom_text(data=cities,aes(x=long, y=lat, label=name), color = "black", check_overlap = TRUE, size = 3) +
  labs(title = "Orders: Top 100 Customer Cities") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank())
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/Cities.png",sep=""), width=13)
#-----------------------------------------------------------------------------------
#age data (weighted)
p1 <- beautify(ggplot(orders, aes(x=Age)) + 
  geom_density() +
  scale_x_continuous(name="Age")) +
  labs(title = "Orders: Age Distribution (weighted by order amount)")

plotData <- na.omit(orders %>% select(Age, Gender))
p2 <-beautify(ggplot(plotData) + 
  stat_density(aes(x=Age, linetype=Gender), geom="line", position="identity") +
  labs(title = "Orders: Age Distribution per Gender (weighted by order amount)") +
  scale_linetype_discrete(guide=guide_legend(override.aes=aes(colour="black"))))
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/Age_weighted.png",sep=""), multiplot(p1,p2, cols=2), width=16, height=5)

#age data
p1 <- beautify(ggplot(ordersDistinctCustomer, aes(x=Age)) + 
                 geom_density() +
                 scale_x_continuous(name="Age")) +
  labs(title = "Orders: Age Distribution")

plotData <- na.omit(ordersDistinctCustomer %>% select(Age, Gender))
p2 <-beautify(ggplot(plotData) + 
                stat_density(aes(x=Age, linetype=Gender), geom="line", position="identity") +
                labs(title = "Orders: Age Distribution per Gender") +
                scale_linetype_discrete(guide=guide_legend(override.aes=aes(colour="black"))))
ggsave(filename=paste(pathPlotFolder,"Customer Data Plots/Age.png",sep=""), multiplot(p1,p2, cols=2), width=16, height=5)
#-----------------------------------------------------------------------------------
#any vehicle
yes <- nrow(subset(orders, Truck.Owner == "True" | Motorcycle.Owner == "True" | RV.Owner == "True"))/nrow(orders)
nan <- nrow(subset(orders, is.na(Truck.Owner) & is.na(Motorcycle.Owner) & is.na(RV.Owner)))/nrow(orders)
no <- (1 - yes - nan)
if (yes > no){
  vehicle <- c('Any Vehicle', paste('True: ', yes, '%'), paste('False:', no, '%'), '', '', '', 'Others: 0%', paste('Not Available:', nan, '%'))
}else{
  vehicle <-  c('Any Vehicle', paste('False:', no, '%'), paste('True: ', yes, '%'), '', '', '', 'Others: 0%', paste('Not Available:', nan, '%'))
}
#-----------------------------------------------------------------------------------
#dataframe for calculated values
calcCol <- data.frame(Variable=character(),
                      Top.1st=character(), 
                      Top.2nd=character(),
                      Top.3rd=character(),
                      Top.4th=character(),
                      Top.5th=character(),
                      Others=character(),
                      Not.Available=character())
calcCol <- rbind(vehicle)
colnames(calcCol) <- c('Variable','Top 1st','Top 2nd','Top 3rd','Top 4th','Top 5th','Others','Not Available')

#print calc columns
print(calcCol)
write.table(calcCol, file = paste(pathTableFolder,"OrderData_Calc.csv"), sep=",", row.names=FALSE)