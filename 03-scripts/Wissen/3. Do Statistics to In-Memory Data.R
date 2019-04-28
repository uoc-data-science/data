#Knowlegde:

#Possible functions used in sapply include mean, sd, var, min, max, median, range, and quantile.
sapply(mydata, mean, na.rm=TRUE)

# mean,median,25th and 75th quartiles,min,max
summary(mydata)

# Tukey min,lower-hinge, median,upper-hinge,max
fivenum(x)