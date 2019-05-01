# get file paths
curDir <- getwd()
filePath <- paste(curDir, "/data/raw/orders", sep="")
dataPath <- file.path(filePath, "order_data.csv")
headersPath <- file.path(filePath, "order_columns.txt")

# prepare column name list
headersFile = file(headersPath, "r")
headerNames <- list()
while(TRUE) {
  line = readLines(headersFile, n = 1)
  if(length(line) == 0){
    break
  }
  headerNames <- append(headerNames, strsplit(line, ":")[[1]][1])
}
close(headersFile)


# read CSV into dataframe and set column names
df <- read.csv(file=dataPath, header=FALSE, sep=",")
colnames(df) <- headerNames