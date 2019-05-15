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

#Plot for BrandNames
top <- (tabyl(clicks$BrandName)) %>% select(1:2) #create frequency table
names(top) <- c("BrandName", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$BrandName <- factor(top$BrandName, levels = top$BrandName) #lockOrder
textXpos <- length(top$amount)/1.2
p1 <- ggplot(top, aes(BrandName, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Brand Name.png",sep=""))

#Plot for Look
top <- (tabyl(clicks$Look)) %>% select(1:2) #create frequency table
names(top) <- c("Look", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$Look <- factor(top$Look, levels = top$Look) #lockOrder
textXpos <- length(top$amount)/1.2
p2 <- ggplot(top, aes(Look, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Look.png",sep=""))

#Plot for BasicOrFashion
top <- (tabyl(clicks$BasicOrFashion)) %>% select(1:2) #create frequency table
names(top) <- c("BasicOrFashion", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$BasicOrFashion <- factor(top$BasicOrFashion, levels = top$BasicOrFashion) #lockOrder
textXpos <- length(top$amount)/1.2
p3 <- ggplot(top, aes(BasicOrFashion, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/BasicOrFashion.png",sep=""))

#Plot for ColorOrScent
top <- (tabyl(clicks$ColorOrScent)) %>% select(1:2) #create frequency table
names(top) <- c("ColorOrScent", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$ColorOrScent <- factor(top$ColorOrScent, levels = top$ColorOrScent) #lockOrder
textXpos <- length(top$amount)/1.2
p4 <- ggplot(top, aes(ColorOrScent, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/ColorOrScent.png",sep=""))

#Plot for Texture
top <- (tabyl(clicks$Texture)) %>% select(1:2) #create frequency table
names(top) <- c("Texture", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$Texture <- factor(top$Texture, levels = top$Texture) #lockOrder
textXpos <- length(top$amount)/1.2
p5 <- ggplot(top, aes(Texture, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Texture.png",sep=""))

#Plot for ToeFeature
top <- (tabyl(clicks$ToeFeature)) %>% select(1:2) #create frequency table
names(top) <- c("ToeFeature", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$ToeFeature <- factor(top$ToeFeature, levels = top$ToeFeature) #lockOrder
textXpos <- length(top$amount)/1.2
p6 <- ggplot(top, aes(ToeFeature, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/ToeFeature.png",sep=""))

#Plot for Material
top <- (tabyl(clicks$Material)) %>% select(1:2) #create frequency table
names(top) <- c("Material", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$Material <- factor(top$Material, levels = top$Material) #lockOrder
textXpos <- length(top$amount)/1.2
p7 <- ggplot(top, aes(Material, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Material.png",sep=""))

#Plot for BodyFeature
top <- (tabyl(clicks$BodyFeature)) %>% select(1:2) #create frequency table
names(top) <- c("BodyFeature", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$BodyFeature <- factor(top$BodyFeature, levels = top$BodyFeature) #lockOrder
textXpos <- length(top$amount)/1.2
p8 <- ggplot(top, aes(BodyFeature, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/BodyFeature.png",sep=""))

#Plot for Product
top <- (tabyl(clicks$Product)) %>% select(1:2) #create frequency table
names(top) <- c("Product", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$Product <- factor(top$Product, levels = top$Product) #lockOrder
textXpos <- length(top$amount)/1.2
p9 <- ggplot(top, aes(Product, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Product.png",sep=""))

#Plot for Manufacturer
top <- (tabyl(clicks$Manufacturer)) %>% select(1:2) #create frequency table
names(top) <- c("Manufacturer", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$Manufacturer <- factor(top$Manufacturer, levels = top$Manufacturer) #lockOrder
textXpos <- length(top$amount)/1.2
p10 <- ggplot(top, aes(Manufacturer, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Manufacturer.png",sep=""))

#Plot for Pattern
top <- (tabyl(clicks$Pattern)) %>% select(1:2) #create frequency table
names(top) <- c("Pattern", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),] #get NAs for added text
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top) #delete NAs from table
top <- arrange(top, desc(amount))
top$Pattern <- factor(top$Pattern, levels = top$Pattern) #lockOrder
textXpos <- length(top$amount)/1.2
p11 <- ggplot(top, aes(Pattern, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = textXpos, y = top[1, 2]/1.2, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Pattern.png",sep=""))

multiplot(p2, p3, p4, p5, p6, p8, p11, cols=2)

ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/ClickstreamColumns.png",sep=""),multiplot(p2, p3, p4, p5, p6, p8, p11, cols=2), width=15, height=10)
