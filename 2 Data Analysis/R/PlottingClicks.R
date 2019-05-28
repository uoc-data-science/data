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
                        "Pattern"
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
#Plots for product-related columns
p2 <- plotOrderedBarChart(clicks, "Look")

p3 <- plotOrderedBarChart(clicks, "BasicOrFashion")

p4 <- plotOrderedBarChart(clicks, "ColorOrScent")

p5 <- plotOrderedBarChart(clicks, "Texture")

p6 <- plotOrderedBarChart(clicks, "ToeFeature")

p7 <- plotOrderedBarChart(clicks, "Material")

p8 <- plotOrderedBarChart(clicks, "BodyFeature")

p9 <- plotOrderedBarChart(clicks, "Product")

p10 <- plotOrderedBarChart(clicks, "BrandName")

p11 <- plotOrderedBarChart(clicks, "Pattern")

p12 <- plotOrderedBarChart(clicks, "Manufacturer")

multiplot(p2, p3, p4, p5, p6, p8, p11, cols=2)
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/ClickstreamColumns.png",sep="")
       ,multiplot(p2, p3, p4, p5, p6, p8, p11, cols=2), width=10, height=15)

#Lorenz curve plots
plotLorenzCurve(clicks, "BrandName")
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/LorenzBrandName.png",sep=""), width=12, height=13)

plotBarAndLorenz(clicks, "Product")
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/LorenzProduct.png",sep=""), width=15, height=12)
