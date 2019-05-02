library(ggplot2)
library(gridExtra)
print(getwd())
useSmallVersions <- TRUE

pathOrders <- "0 Data/order_data_cleaned_R.csv"
pathClicks  <- "0 Data/clickstream_data_cleaned_R.csv"
if (useSmallVersions){
  pathOrders <- "0 Data/order_data_small_R.csv"
  pathClicks <- "0 Data/clickstream_data_small_R.csv"
}

pathPlotFolder <- "./4 Data Visualization/"
pathDistributionPrefix <- "./4 Data Visualization/Distribution Plots/"
# pathRankingPrefix 

orders <- read.csv(file=pathOrders)
clicks <- read.csv(file=pathClicks)


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
  scale_x_continuous(limits = c(0.5,7.5),breaks = round(seq(1, 7, by = 1),1))
ggsave(filename=paste(pathPlotFolder,"Order Line Quantity.png"))

# Distribution of order amount
p1 <- ggplot(orders, aes(x=Order.Line.Amount)) + 
  geom_density() +
  scale_x_continuous(limits = c(-0.5,45.5),breaks = round(seq(0, 45, by = 3),1))
p2 <- ggplot(orders, aes(x=Order.Line.Amount)) +
  geom_histogram(binwidth=1, colour="black", fill="white") +
  scale_x_continuous(limits = c(-2.5,45.5),breaks = round(seq(0, 45, by = 3),1))
# Distribution of order unit price
p3 <- ggplot(orders, aes(x=Order.Line.Unit.List.Price)) + 
  geom_density() +
  scale_x_continuous(limits = c(-0.5,45.5),breaks = round(seq(0, 45, by = 3),1))
p4 <- ggplot(orders, aes(x=Order.Line.Unit.List.Price)) +
  geom_histogram(binwidth=1, colour="black", fill="white") +
  scale_x_continuous(limits = c(-2.5,45.5),breaks = round(seq(0, 45, by = 3),1))
multiplot(p1, p2, p3, p4, cols=2)
ggsave(filename=paste(pathPlotFolder,"Order Price.png"),multiplot(p1, p2, p3, p4, cols=2), width=15)
