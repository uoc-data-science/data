library(gridExtra)
library(dplyr)
library(janitor)
library(maps)
library(ggplot2)
library(tidyr)
library(ineq)
library(ggrepel) #for advanced labeling possibilities

#fontSize
centralSize = 12
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#functions
#-----------------------------------------------------------------------------------
#count different values in column of dataframe (calculate ratio)
giveTop <- function(df, column, first, percentage){
  top <- (tabyl(df[[column]])) #create frequency table
  top[,c(1)] <- as.character(top[,c(1)])
  top[is.na(top)] <- "Not Available"
  naRow <- top %>%
    filter(top[[1]] == "Not Available")
  naRow <- naRow[,1:3]
  if (nrow(naRow)>0){naRow[1,3] <- round(naRow[1,3]*100, digits = 2)}
  
  totalCount <- sum(top$n) # used when only the top x columns are requested
  if (percentage == TRUE){
    df <- subset(df, select=c(column))
    df <- na.omit(df)

    top <- (tabyl(df[[column]])) #create frequency table
    
    top[,c(1)] <- as.character(top[,c(1)])
    
    top <- top %>% select(1, 3)
    names(top) <- c(column, "percentage") #rename
    top$percentage <- round(top$percentage*100, digits = 2)
    top <- arrange(top, desc(percentage))
  }
  else{
    top <- top %>% select(1:2)
    names(top) <- c(column, "amount") #rename
    top <- arrange(top, desc(amount))
  }
  #get the highest rating x columns only
  if (first != 0){
    top <- head(top, first)
    if(percentage == TRUE){
      others <- max((100 - sum(top$percentage)),0) #Possible bug without max: others < 0 due to rounding
      top[nrow(top) + 1,] = list("Others",others)
      if (nrow(naRow)>0){
        top[nrow(top) + 1,] = naRow %>% select(1, 3)
      }
      top <- arrange(top, desc(percentage))
    }
    else{
      others <- (totalCount-sum(top$amount))
      top[nrow(top) + 1,] = list("Others",others)
      top <- arrange(top, desc(amount))
    }
  }
  else {
    if (percentage==TRUE){
      if (nrow(naRow)>0){
        top[nrow(top) + 1,] = naRow %>% select(1, 3)
      }
      top <- arrange(top, desc(percentage))
    }
  }
  top[[column]] <- factor(top[[column]], levels=top[[column]]) #lockOrder
  return(top)
}
#-----------------------------------------------------------------------------------
#simple bar plot
plotBar <- function(df, xAxis, yAxis, filename, title, size){
  ggplot(df, aes_string(x = xAxis, y = yAxis)) +
    geom_bar(stat="identity") +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename=paste(pathPlotFolder,filename,sep=""), width=size)
}
#-----------------------------------------------------------------------------------
#calculate percentages from different bool columns
boolToBar <- function(df, columnList, labelList, xLabel, yLabel){
  plotData <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(plotData) <- c("column_of_interest", "percentage")
  i <- 1
  for (sector in columnList){
    top <- tabyl(df[,sector], sort = TRUE, show_na = FALSE)
    colnames(top) <- c(sector,"n","percent")
    percentage <- (round(top[2,"percent"],2))*100
    plotData[nrow(plotData) + 1,] = list(labelList[[i]],percentage)
    i <- i+1
  }
  plotData <- plotData %>% arrange(desc(percentage))
  plot <- ggplot(plotData, aes(x=reorder(column_of_interest,-percentage),y=percentage)) +
    geom_bar(stat="identity") +
    scale_x_discrete(name=xLabel) +
    scale_y_continuous(name=yLabel) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(plot)
}
#-----------------------------------------------------------------------------------
#Returns a plot with a mean of a given column for each day
meanOverTime <- function(df, dateColumn, columnOfInterest, yLabel){
  plotData <- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("Date", "Mean")
  colnames(plotData) <- x
  for (day in df[[dateColumn]]){
    if (!substring(day,6) %in% plotData$Date){
      subset <- df %>% filter(!!as.name(dateColumn) == day)
      meanValue = mean(subset[[columnOfInterest]])
      plotData[nrow(plotData) + 1,] = list(substring(day,6),meanValue)
    }
  }
  plotData <- plotData %>% arrange(Date)
  
  plot <- ggplot(data=plotData, aes(x=Date, y=Mean, group=1)) +
    geom_line() +
    geom_point() +
    scale_x_discrete(breaks = c(head(plotData,1)$Date, tail(plotData,1)$Date)) +
    scale_y_continuous(name=yLabel)
  return(plot)
}
#-----------------------------------------------------------------------------------
#Ordered bar chart function
plotOrderedBarChart <- function(df, ColumnName) {
  top <- (tabyl(df[, ColumnName])) %>% select(1:2) #create frequency table
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
    annotate("text", x = Inf, y = Inf, label = nas, vjust=1, hjust=1)
}
#-----------------------------------------------------------------------------------
#Lorenz curve for a column
plotLorenzCurve <- function(df, ColumnName) {
  # Compute the Lorenz curve Lc{ineq}
  top <- df[, ColumnName] %>% na.omit() %>% (tabyl) %>% select(1,3) #create frequency table
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
  ggplot(data=LorenzFinal_df, aes_string(x=ColumnName, y="percentage", group=1, axes=F)) +
    geom_point() +
    geom_line() +
    geom_abline(slope = 1/(countOfEntries-1), intercept = -1/(countOfEntries-1))
}
#-----------------------------------------------------------------------------------
#bar plot and lorenz curve for a column
plotBarAndLorenz <- function(df, ColumnName) {
  top <- df[, ColumnName] %>% na.omit() %>% (tabyl)#create frequency table
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
    theme_classic() +
    geom_bar(aes_string(ColumnName, "amount"), fill = "#BDBDBD", width=.8, stat="identity") +
    geom_point(aes_string(x=ColumnName, y=top$percentage*largest_amount, group=1)) +
    geom_line(aes_string(x=ColumnName, y=top$percentage*largest_amount, group=1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
    geom_abline(slope = 1/(countOfEntries)*largest_amount, intercept = 0) +
    scale_y_continuous(sec.axis = sec_axis(~./largest_amount))
}

#-----------------------------------------------------------------------------------
#summary function for numerical columns of tables
summarizeNumericalColumns <- function(df){
  df <- select_if(df, is.numeric)
  
  summary_stats <- data.frame(matrix(ncol = 6, nrow = 0))
  x <- c("Variable","Max", "Mean", "Median", "Min", "SD")
  colnames(summary_stats) <- x
  
  for (column in names(df)){
    maxValue <- max(df[[column]], na.rm = TRUE)
    meanValue <- round(mean(df[[column]], na.rm = TRUE),digits=2)
    medianValue <- median(df[[column]], na.rm = TRUE)
    minValue <- min(df[[column]], na.rm = TRUE)
    sdValue <- round(sd(df[[column]], na.rm = TRUE),digits=2)
    summary_stats[nrow(summary_stats) + 1,] = list(column,maxValue,meanValue,medianValue,minValue,sdValue)
  }
  return(summary_stats)
}
#-----------------------------------------------------------------------------------
#summary function for factor columns of tables
summarizeFactorColumns <- function(df){
  df <- select_if(df, is.factor)
  
  summary_stats <- data.frame(matrix(ncol = 8, nrow = 0))
  x <- c("Variable","Top 1st","Top 2nd", "Top 3rd", "Top 4th", "Top 5th", "Others","Not Available")
  colnames(summary_stats) <- x
  
  for (column in names(df)){
    top <- giveTop(df,column,5,TRUE)
    Others <- paste("Others: ",round(top[top[,1]=="Others",][1,2], digits = 2),"%",sep="") # Use round to fix a bug where digits are added
    NotAvailable <- "Not Available: 0%"
    if ("Not Available" %in% top[,1]){
      NotAvailable <- paste("Not Available: ",top[top[,1]=="Not Available",][1,2],"%",sep="")
    }
    #Subset witohut entry for Others or NA
    pureTop <- top[top[,1]!="Others" & top[,1]!="Not Available",]
    summary <- c(column,"","","","","",Others,NotAvailable)
    loopStop <- min(10,nrow(pureTop))
    for (i in 1:loopStop){
      summary[i+1] <- paste(pureTop[i,1],": ",pureTop[i,2],"%",sep="")
    }
    summary_stats[nrow(summary_stats) + 1,] <- summary
  }
  summary_stats[['Others']] <- gsub('Others: ', '', summary_stats[['Others']])
  summary_stats[['Not Available']] <- gsub('Not Available: ', '', summary_stats[['Not Available']])
  return(summary_stats)
}
#-----------------------------------------------------------------------------------
# beautify plots
beautify <- function(plot){
  plot <- plot +
    theme_classic() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"))
  return(plot)
}
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
# Utility function for subplot support, Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#-----------------------------------------------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title="") {
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