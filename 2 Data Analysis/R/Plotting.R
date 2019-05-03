library(ggplot2)
library(gridExtra)

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
interestingColumns <- c("Order.Line.Quantity","Order.Line.Unit.List.Price",
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
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$col,
                                      layout.pos.col = matchidx$row))
    }
  }
}



# Distribution of order line quantity
ggplot(orders, aes(x=Order.Line.Quantity)) +
  geom_histogram(binwidth=1, colour="black", fill="white") +  # Overlay with transparent density plot
  geom_vline(aes(xintercept=mean(Order.Line.Quantity, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(name ="Order Line Quantity",limits = c(0.5,7.5),breaks = round(seq(1, 7, by = 1),1))
ggsave(filename=paste(pathPlotFolder,"Order Line Quantity.png"))

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
multiplot(p1, p2, p3, p4, cols=2)
ggsave(filename=paste(pathPlotFolder,"Order Price.png"),multiplot(p1, p2, p3, p4, cols=2), width=15)
