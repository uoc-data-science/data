library(gridExtra)
library(dplyr)
library(janitor)
library(ggplot2)

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

#Plots for product-related columns
p1 <- plotOrderedBarChart("BrandName")

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

