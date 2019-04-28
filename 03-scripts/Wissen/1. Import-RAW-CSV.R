# this sript loads the content of a CSV-file into the variable mydata
# the sep (seperator) is set to semicolon
# TODo: Not sure what header = TRUE means or used for...

mydata <- read.table("_RAW/RAW.csv", header=TRUE, sep=";")

# alternative: erfordert aber library
# read.csv