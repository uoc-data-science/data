library(gridExtra)
library(dplyr)
library(janitor)
library(ggplot2)
library(ineq)
source("./2 Data Analysis/R/PlottingUtilityFunctions.R")

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
                        "Pattern",
                        "REQUEST_HOUR_OF_DAY"
)
subset <- subset(clicks, select=interestingColumns)
facCol <- summarizeFactorColumns(subset)
numCol <- summarizeNumericalColumns(subset)
write.table(facCol, file = paste(pathTableFolder,"ClickstreamData_Factors.csv"), sep=",", row.names=FALSE)
write.table(numCol, file = paste(pathTableFolder,"ClickstreamData_Numerical.csv"), sep=",", row.names=FALSE)

#------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Plotting
#-----------------------------------------------------------------------------------
#Lorenz curve plots
plotBarAndLorenz(clicks, "BrandName")
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/LorenzBrandName.png",sep=""), width=12, height=13)

plotBarAndLorenz(clicks, "Product")
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/LorenzProduct.png",sep=""), width=15, height=12)

#Density Hour of Day
beautify(ggplot(clicks, aes(x=REQUEST_HOUR_OF_DAY)) +
           geom_density() +
           scale_x_continuous(name="Hour of Day")) +
  ggtitle("Density of Clicks over Hour of Day")
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Click Time.png",sep=""), width=10)

#Density of time between first and recent click

#History of Clicks
