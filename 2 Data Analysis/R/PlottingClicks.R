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
write.table(summary, file = paste(pathTableFolder,"ClickstreamData.csv"), sep=",", row.names=FALSE)

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

# Distribution of brand names
ggplot(data=subset(clicks, !is.na(BrandName)), aes(BrandName)) +
  geom_bar(width=.5, fill="tomato3") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Brand Name Quantity.png",sep=""))

#Plot for BrandNames
top <- (tabyl(clicks$BrandName)) %>% select(1:2) #create frequency table
names(top) <- c("BrandName", "amount") #rename
top[,c(1)] <- as.character(top[,c(1)])
nas <- top[is.na(top),]
nas <- nas$amount
nas <- paste0("Amount of NAs: ", nas)
top <- na.omit(top)
top <- arrange(top, desc(amount))
top$BrandName <- factor(top$BrandName, levels = top$BrandName) #lockOrder
ggplot(top, aes(BrandName, amount), y=amount) +
  geom_bar(width=.8, fill="tomato3", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
  annotate("text", x = 20, y = 200, label = nas)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Brand Name.png",sep=""))