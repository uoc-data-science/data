library(gridExtra)
library(dplyr)
library(janitor)
library(ggplot2)
library(ineq)
library(chron)
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
#Lorenz Curve Brands
plotBarAndLorenz(clicks, "BrandName")
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Lorenz Brands.png",sep=""), width=12, height=13)
#-----------------------------------------------------------------------------------
#Lorenz Curve Products
plotBarAndLorenz(clicks, "Product")
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Lorenz Products.png",sep=""), width=15, height=12)
#-----------------------------------------------------------------------------------
#Density Hour of Day
beautify(ggplot(clicks, aes(x=REQUEST_HOUR_OF_DAY)) +
           geom_density() +
           scale_x_continuous(name="Hour of Day")) +
  ggtitle("Density of Clicks over Hour of Day")
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Click Time.png",sep=""), width=10)
#-----------------------------------------------------------------------------------
#Density of time between first and recent click
SeqAndTime <- clicks %>% # select sequence and time
  select("Request.Date_Time", "Request.Sequence")

SeqPos <- 0 # the position in the click sequence
Index <- 1 # current index
FirstVisit <- 1 # position of first click
vector <- character(0) # empty helper vector

# search for start and and of click sequences and get the matching times
  for (val in SeqAndTime$Request.Sequence) {
    if(val>SeqPos) {
      SeqPos <- val
    }
    if(val<SeqPos) {
      vector <- c(vector, SeqAndTime[FirstVisit, "Request.Date_Time"] %>%
                    as.character())
      vector <- c(vector, SeqAndTime[Index-1, "Request.Date_Time"] %>%
                    as.character())
      FirstVisit <- Index
      SeqPos <- 1
    }
    Index <- Index+1
  }

vector <- chron(times=vector) # convert the times into chron objects
diffV <- diff(vector) %>%
  as.data.frame # compute the difference between sequential times
diffV <- diffV[seq(1, nrow(diffV), 2), 1] %>%
  as.data.frame # only select every second entry
diffV$hours <- hours(diffV$.) # add columns for hours, minutes and seconds
diffV$minutes <- minutes(diffV$.)
diffV$seconds <- seconds(diffV$.)
diffV$totalMinutes <- diffV$hours*60 + #calculate the total minutes
  diffV$minutes +
  diffV$seconds/60
calc <- data.frame("Session Duration" = diffV$totalMinutes)
diffV <- subset(diffV, totalMinutes<40) # only select rows with less than 40 mins
                                        # for visual reasons
print(diffV)


# plot the data
beautify(ggplot(diffV, aes(totalMinutes)) +
  geom_density() +
  ggtitle("Density of Session Duration (under 40 minutes)"))
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Session Duration.png",sep=""), width=10)
#-----------------------------------------------------------------------------------
#History of Clicks
hourGroup <- clicks %>%
  select(Request.Date, REQUEST_HOUR_OF_DAY) %>%
  group_by(Request.Date, REQUEST_HOUR_OF_DAY) %>%
  summarize(count = n())

hourGroup$datetime <- paste0(hourGroup$REQUEST_HOUR_OF_DAY, ":00:00")
hourGroup$datetime <- paste(hourGroup$Request.Date, hourGroup$datetime)
hourGroup$datetime <- as.POSIXct(hourGroup$datetime, format = "%Y-%m-%d %H:%M:%S")  
   
beautify(ggplot(hourGroup, aes(x = datetime, y=count)) +
  geom_line() +
  ggtitle("History of Clicks per Hour"))
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Click History.png",sep=""), width=10)
#-----------------------------------------------------------------------------------
#print calc columns
calcCol <- summarizeNumericalColumns(calc)
print(calcCol)
write.table(calcCol, file = paste(pathTableFolder,"ClickstreamData_Calc.csv"), sep=",", row.names=FALSE)