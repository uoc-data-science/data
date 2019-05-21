library(gridExtra)
library(dplyr)
library(janitor)
library(maps)
library(ggplot2)
library(tidyr)
library(ggrepel) #for advanced labeling possibilities


#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#functions
#-----------------------------------------------------------------------------------
#count different values in column of dataframe (calculate ratio)
giveTop <- function(df, column, first, percentage){
  top <- (tabyl(df[[column]])) #create frequency table
  top[,c(1)] <- as.character(top[,c(1)])
  top[is.na(top)] <- "Not Available"
  totalCount <- sum(top$n) # used when only the top x columns are requested
  if (percentage == TRUE){
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
      others <- (100 - sum(top$percentage))
      top[nrow(top) + 1,] = list("Others",others)
      top <- arrange(top, desc(percentage))
    }
    else{
      others <- (totalCount-sum(top$amount))
      top[nrow(top) + 1,] = list("Others",others)
      top <- arrange(top, desc(amount))
    }
    
  }
  top[[column]] <- factor(top[[column]], levels=top[[column]]) #lockOrder
  return(top)
}

#simpl bar plot
plotBar <- function(df, xAxis, yAxis, filename, title, size){
  ggplot(df, aes_string(x = xAxis, y = yAxis)) +
    geom_bar(stat="identity") +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename=paste(pathPlotFolder,filename,sep=""), width=size)
}

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
#-----------------------------------------------------------------------------------

# Utility function for subplot support, Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
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
