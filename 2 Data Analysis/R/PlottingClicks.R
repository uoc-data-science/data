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

#read mapping table for US states plotting
usStates <- read.csv(file="2 Data Analysis/states.csv")

# read data
clicks <- read.csv(file=pathClicks)
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
# Overview table of interestng columns
#-----------------------------------------------------------------------------------
clicksCustomer <- c("City",
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

selectUniqueCustomer <- clicks[ which(clicks$Request.Sequence==1), ]
subset <- subset(clicks, select=clicksCustomer)
facCol <- summarizeFactorColumns(subset)
numCol <- summarizeNumericalColumns(subset)
write.table(facCol, file = paste(pathTableFolder,"ClickstreamDataCustomer_Factors_Short.csv"), sep=",", row.names=FALSE)
write.table(numCol, file = paste(pathTableFolder,"ClickstreamDataCustomer_Numerical_Short.csv"), sep=",", row.names=FALSE)

clicksProducts <- c("StockType",
                   "Manufacturer",
                   "BrandName"
)

subset <- subset(clicks, select=clicksProducts)
facCol <- summarizeFactorColumns(subset)
numCol <- summarizeNumericalColumns(subset)
write.table(facCol, file = paste(pathTableFolder,"ClickstreamDataProducts_Factors_Short.csv"), sep=",", row.names=FALSE)
write.table(numCol, file = paste(pathTableFolder,"ClickstreamDataProducts_Numerical_Short.csv"), sep=",", row.names=FALSE)

clicksTime <- c("Request.Date",
                    "REQUEST_DAY_OF_WEEK",
                    "REQUEST_HOUR_OF_DAY",
                    "Session.Visit.Count"
)

selectUniqueCustomer <- clicks[ which(clicks$Request.Sequence==1), ]
subset <- subset(clicks, select=clicksTime)

datem <- subset %>%
  mutate(Request.Date=as.Date(Request.Date, format = "%Y-%m-%d")) %>%
  summarise(min = min(Request.Date), max = max(Request.Date))

dateList <- c("Request.Date", as.character(datem$max[1]), "", "", as.character(datem$min[1]), "")
numCol <- summarizeNumericalColumns(subset)
numCol <- rbind(numCol, dateList) 
facCol <- summarizeFactorColumns(subset)
write.table(facCol, file = paste(pathTableFolder,"ClickstreamTime_Factors_Short.csv"), sep=",", row.names=FALSE)
write.table(numCol, file = paste(pathTableFolder,"ClickstreamTime_Numerical_Short.csv"), sep=",", row.names=FALSE)

#------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Plotting
#-----------------------------------------------------------------------------------
#Lorenz Curve Brands
beautify(plotLorenzCurve(clicks, "BrandName") +
        ggtitle("Lorenz Curve Brands")) +
        theme(axis.text.x = element_blank(), axis.text.y = element_blank())
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/LorenzBrands.png",sep=""), width=5, height=5)
#-----------------------------------------------------------------------------------
#Lorenz Curve Products
beautify(plotLorenzCurve(clicks, "Product") +
        ggtitle("Lorenz Curve Products")) +
        theme(axis.text.x = element_blank(), axis.text.y = element_blank())  
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/LorenzProducts.png",sep=""), width=5, height=5)
#-----------------------------------------------------------------------------------
#Lorenz Curve Manufacturers
beautify(plotLorenzCurve(clicks, "Manufacturer") +
           ggtitle("Lorenz Curve Products")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())  
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/LorenzManufacturers.png",sep=""), width=5, height=5)
#-----------------------------------------------------------------------------------
#Density Hour of Day
beautify(ggplot(clicks, aes(x=REQUEST_HOUR_OF_DAY)) +
           geom_density() +
           scale_x_continuous(name="Hour of Day")) +
  ggtitle("Density of Clicks over Hour of Day")
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/ClickTime.png",sep=""), width=10)
#-----------------------------------------------------------------------------------
#Density of time between first and recent click
SeqAndTime <- clicks %>% # select sequence and time
  select("Request.Date_Time", "Request.Sequence")

SeqPos <- 0 # the position in the click sequence
Index <- 1 # current index
FirstVisit <- 1 # position of first click
vector <- character(0) # empty helper vector
clickNum <- vector()
# search for start and and of click sequences and get the matching times
  for (val in SeqAndTime$Request.Sequence) {
    if(val>SeqPos) {
      SeqPos <- val
    }
    if(val<SeqPos) {
      clickNum <- c(clickNum, SeqPos)
      vector <- c(vector, SeqAndTime[FirstVisit, "Request.Date_Time"] %>%
                    as.character())
      vector <- c(vector, SeqAndTime[Index-1, "Request.Date_Time"] %>%
                    as.character())
      FirstVisit <- Index
      SeqPos <- 1
    }
    if(val == SeqPos) {
      clickNum <- c(clickNum, SeqPos)
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
clickNum <- data.frame("Click Number" = clickNum)
diffV <- subset(diffV, totalMinutes<40) # only select rows with less than 40 mins
                                        # for visual reasons
print(diffV)


# plot the data
beautify(ggplot(diffV, aes(totalMinutes)) +
  geom_density() +
  ggtitle("Density of Session Duration (under 40 minutes)"))
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/SessionDuration.png",sep=""), width=10)
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
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/ClickHistory.png",sep=""), width=10)
#-----------------------------------------------------------------------------------
# US States heatmap
states <- usStates
states$State <- tolower(states$State)
gusa <- map_data("state")
plotData <- merge(clicks, states, by.x="US.State", group = group, by.y="Abbreviation")
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
  labs(x = NULL, y = NULL, fill = "Count", title = "Customer Hotspots (weighted by traffic)") +
  scale_fill_gradient(low = "white", high = "black") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank()) +
  #geom_label_repel(data = centroids, aes(x, y, label=state), force=2, segment.color="#fd9409", nudge_x=0.5) +
  coord_map()
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/States_weighted.png",sep=""), width=13)
#-----------------------------------------------------------------------------------
#most important cities (map)
states <- usStates %>% filter(State != "Alaska")
plotData <- merge(clicks, states, by.x="US.State", by.y="Abbreviation")
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
  labs(title = "Top 100 Customer Cities (weighted by traffic)") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank())
ggsave(filename=paste(pathPlotFolder,"Clickstream Data Plots/Cities_weighted.png",sep=""), width=15)
#-----------------------------------------------------------------------------------
#print calc columns
calcCol <- summarizeNumericalColumns(calc)
calcCol2 <- summarizeNumericalColumns(clickNum)
calcCol <- rbind(calcCol, calcCol2)

write.table(calcCol, file = paste(pathTableFolder,"ClickstreamData_Calc.csv"), sep=",", row.names=FALSE)