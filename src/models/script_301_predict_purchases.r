library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)


message("\n\n\nLoading data...")
orderDf <- read.csv(file="data/interim/orders/orders_cleaned.csv", sep=",", na.strings=c("?","NA"))
partialOrdersDf <- orderDf[c("Product_Family_ID",
                             "Order_Line_Day_of_Week",
                             "Order_Line_Hour_of_Day",
                             "Product_ID",
                             "Gender",
                             "US_State",
                             "Age")]


# force certain variables to be categorical:
partialOrdersDf$Product_Family_ID <- as.factor(partialOrdersDf$Product_Family_ID)
partialOrdersDf$Order_Line_Day_of_Week <- as.factor(partialOrdersDf$Order_Line_Day_of_Week)
partialOrdersDf$Order_Line_Hour_of_Day <- as.factor(partialOrdersDf$Order_Line_Hour_of_Day)
partialOrdersDf$Gender <- as.factor(partialOrdersDf$Gender)
partialOrdersDf$US_State <- as.factor(partialOrdersDf$US_State)
partialOrdersDf$Age <- as.numeric(partialOrdersDf$Age)
summary(partialOrdersDf)


message("\n\n\nTraining the model...")

# We first grow a large tree with a complexity parameter of cp=0.0001 (i.e. splits must decrease
# the lack of fit by factor cp=0.0001):
tree <- rpart(Product_Family_ID ~ Order_Line_Day_of_Week + Gender + Age,
              method="class",
              data=partialOrdersDf,
              cp=0.0001)

# print cross-validation results:
printcp(tree)

# We than examine the results of a 10-fold cross-validation by plotting the complexity
# parameter vs. its cross-validated error:
plotcp(tree)

# To prevent over-fitting, we choose the leftmost cp value below the dotted line (cp=0.001): a tree
# of that size (here: 36 splits) is the smallest tree whose cross-validated error is whithin one standard
# error of the minimum cross-validated error value. We than prune the tree to its new size:
prunedTree <- prune(tree, cp=0.001)

# And pretty-print the resulting tree:
prp(prunedTree)

# TODO:
# Product_Family_ID ~ Order_Line_Day_of_Week + Order_Line_Hour_of_Day,
# ...
# cp = 0.001)
# => 83 splits


#TODO:
# enable cross validation and set the number of folds:
#ctrl <- trainControl(method = "cv",
#                     number = 10)

# Can certain variables predict the Product_Family_ID?
# try a logistic regression:
#productFamilyIdModel1 <- train(Product_Family_ID ~ Order_Line_Day_of_Week + Order_Line_Hour_of_Day,
#                               data = partialOrdersDf,
#                               trControl = ctrl,
#                               method = "glm")
#productFamilyIdModel1
