library(gridExtra)
library(dplyr)
library(janitor)
library(ggplot2)
library(ineq)

useSmallVersions <- FALSE

# decide whether to use dev data or not
pathClicks  <- "0 Data/clickstream_data_cleaned_R.csv"
if (useSmallVersions){
  pathClicks <- "0 Data/clickstream_data_small_R.csv"
}

# target path
pathPlotFolder <- "./4 Data Overview/Plots/"
pathTableFolder <- "./4 Data Overview/Tables/"

# read data
clicks <- read.csv(file=pathClicks)

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
# Overview table of interestng columns
#-----------------------------------------------------------------------------------
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
#write.table(summary, file = paste(pathTableFolder,"ClickstreamData.csv"), sep=",", row.names=FALSE)

#-----------------------------------------------------------------------------------
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
#Ordered bar chart function
plotOrderedBarChart <- function(ColumnName) {
  top <- (tabyl(clicks[, ColumnName])) %>% select(1:2) #create frequency table
  names(top) <- c(ColumnName, "amount") #rename
  top[,c(1)] <- as.character(top[,c(1)])
  nas <- top[is.na(top),] #get NAs for added text
  nas <- nas$amount
  nas <- paste0("Amount of NAs: ", nas)
  top <- na.omit(top) #delete NAs from table
  top <- arrange(top, desc(amount))
  top[, ColumnName] <- factor(top[, ColumnName], levels = top[, ColumnName]) #lockOrder
  textXpos <- length(top$amount)/1.2
  ggplot(top, aes_string(ColumnName, "amount"), y=amount) +
    geom_bar(width=.8, fill="tomato3", stat="identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
    annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
}

#-------------------------------------
plotLorenzCurve <- function(ColumnName) {
  # Compute the Lorenz curve Lc{ineq}
  top <- clicks[, ColumnName] %>% na.omit() %>% (tabyl) %>% select(1,3) #create frequency table
  names(top) <- c(ColumnName, "percentage") #rename
  top[,c(1)] <- as.character(top[,c(1)])
  top <- arrange(top, -percentage) #sort by descending percentage
  top[, ColumnName] <- factor(top[, ColumnName], levels = top[, ColumnName]) #lockOrder
  
  LorenzCurve <- Lc(top$percentage) #calculate the Lorenz curve
  
  # create data.frame from LC
  LorenzCurve_df <- data.frame(p = LorenzCurve$p, L = rev(1-LorenzCurve$L)) #create datafram from calculation
  ObsZero_df <- data.frame(ColumnName = "", percentage = 0) #create dataframe with an empty observation
  names(ObsZero_df) <- c(ColumnName, "percentage") #rename the columns
  LorenzFinal_df <- rbind(ObsZero_df, top) #add the original dataframe
  LorenzFinal_df$percentage <- LorenzCurve_df$L #replace the percentage column with entries from calculation 
  countOfEntries <- LorenzFinal_df[, ColumnName] %>% tabyl() %>% select(2) %>% sum() #count the entries for visual reasons
  # plot
  ggplot(data=LorenzFinal_df, aes_string(x=ColumnName, y="percentage", group=1)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
    geom_abline(slope = 1/(countOfEntries-1), intercept = -1/(countOfEntries-1))
}

plotBarAndLorenz <- function(ColumnName) {
  top <- clicks[, ColumnName] %>% na.omit() %>% (tabyl)#create frequency table
  names(top) <- c(ColumnName,"amount", "percentage") #rename
  top[,c(1)] <- as.character(top[,c(1)])
  top <- arrange(top, -amount) #sort by descending percentage
  top[, ColumnName] <- factor(top[, ColumnName], levels = top[, ColumnName]) #lockOrder
  
  LorenzCurve <- Lc(top$percentage) #calculate the Lorenz curve
  
  # create data.frame from LC
  LorenzCurve_df <- data.frame(p = LorenzCurve$p, L = rev(1-LorenzCurve$L)) #create datafram from calculation
  names(LorenzCurve_df) <- c(ColumnName, "percentage") #rename the columns
  LorenzCurve_df <- LorenzCurve_df[-1,]
  top$percentage <- LorenzCurve_df$per #add the original dataframe
  countOfEntries <- top[, ColumnName] %>% tabyl() %>% select(2) %>% sum() #count the entries for visual reasons
  largest_amount <- top[1,"amount"]
  
  ggplot(data=top) +
    geom_bar(aes_string(ColumnName, "amount"), width=.8, fill="tomato3", stat="identity") +
    geom_point(aes_string(x=ColumnName, y=top$percentage*largest_amount, group=1)) +
    geom_line(aes_string(x=ColumnName, y=top$percentage*largest_amount, group=1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
    geom_abline(slope = 1/(countOfEntries)*largest_amount, intercept = 0) +
    scale_y_continuous(sec.axis = sec_axis(~./largest_amount))
}

#------------------------------------------------------------------------------

#Plots for product-related columns
p1
p2 <- plotOrderedBarChart("Look")

p3 <- plotOrderedBarChart("BasicOrFashion")

p4 <- plotOrderedBarChart("ColorOrScent")

p5 <- plotOrderedBarChart("Texture")

p6 <- plotOrderedBarChart("ToeFeature")

p7 <- plotOrderedBarChart("Material")

p8 <- plotOrderedBarChart("BodyFeature")

p9 <- plotOrderedBarChart("Product")

p10 <- plotOrderedBarChart("BrandName")

p11 <- plotOrderedBarChart("Pattern")

p12 <- plotOrderedBarChart("Manufacturer")

multiplot(p2, p3, p4, p5, p6, p8, p11, cols=2)

ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/ClickstreamColumns.png",sep="")
       ,multiplot(p2, p3, p4, p5, p6, p8, p11, cols=2), width=10, height=15)

#Lorenz curve plots
plotLorenzCurve("BrandName")

plotBarAndLorenz("Product")

