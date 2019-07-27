library(ggplot2)
library(ggthemes)
library(stringr)
library(plyr)

cleanclickstream <- read.csv("02-clean-data/clickstream/clickstream_data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
cleanorders <- read.csv("02-clean-data/orders/order_data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

a <- count(cleanclickstream$`Product.ID`)
colnames(a) <- c("ProductID", "AmountVisits")
a <- a[!a[,1]=="?",]
a <- transform(a, RatioVisits = round(a$AmountVisits*100/sum(a$AmountVisits), 2))

b <- count(cleanorders$`Product.ID`)
colnames(b) <- c("ProductID", "AmountPurchases")
b <- b[!b[,1]=="?",]

popular_products <- left_join(a, b, by ="ProductID", match = "")
popular_products$AmountPurchases[is.na(popular_products$AmountPurchases)] <- 0
popular_products <- transform(popular_products, RatioSales = (popular_products$AmountPurchases*100/sum(popular_products$AmountPurchases)))
popular_products$RatioSales[is.na(popular_products$RatioSales)] <- 0


c <- select(cleanclickstream, `Product.Code`, `Product.ID`)
c <- c[!c[,1]=="NULL",]

colnames(c) <- c("ProductName", "ProductID")
c <- c[!duplicated(c),]

popular_products <- left_join(popular_products, c, by ="ProductID", match = "first")
int_prod <- left_join(a, b, by ="ProductID", match = "")
popular_products <- popular_products[order(popular_products$AmountPurchases, decreasing = FALSE), ]

popular_products <- popular_products[!popular_products$RatioSales<1, ]
popular_products$ProductName <- factor(popular_products$ProductName, levels = popular_products$ProductName)



popular_products <- transform(popular_products, RatioSales = round(popular_products$AmountPurchases*100/sum(popular_products$AmountPurchases), 2))
popular_products <- transform(popular_products, RatioVisitSales = round(popular_products$AmountPurchases*100/popular_products$AmountVisits), 2)


int_prod$AmountPurchases[is.na(int_prod$AmountPurchases)] <- 0
int_prod <- transform(int_prod, RatioSales = round(int_prod$AmountPurchases*100/sum(int_prod$AmountPurchases), 2))
int_prod$RatioSales[is.na(int_prod$RatioSales)] <- 0
int_prod <- transform(int_prod, RatioVisitsPurchases = (int_prod$AmountPurchases/int_prod$AmountVisits))

int_prod <- left_join(int_prod, c, by ="ProductID", match = "first")
int_prod <- int_prod[order(int_prod$AmountVisits, decreasing = TRUE), ]
int_prod$ProductName <- factor(int_prod$ProductName, levels = int_prod$ProductName)

int_prod <- int_prod[1:20,]
a <- select(int_prod, ProductName, RatioVisits, RatioVisitsPurchases)
a <- gather(a, "Legend", "Value", (2:3))
popular_products <- popular_products[1:20,]

m <- nrow(popular_products)

ggplot(popular_products, aes(x=popular_products$ProductName, y=popular_products$AmountPurchases, label= 1:m)) +
  geom_bar(stat="identity") +
  coord_flip()+
  labs(title="Top ten products", 
       subtitle=paste("Out of", nrow(cleanorders), "sales")) +
  xlab("Product Name") +
  ylab("Absolute purchases") +
  theme(legend.position = "right")
