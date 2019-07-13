


buffer<- order_df %>%
  select(Order_Customer_ID,Order_ID,Order_Line_ID,
         Order_Line_Quantity,Product_ID,Order_Date,Order_Promotion_Code,
         Order_Discount_Amount,Order_Line_Unit_List_Price,Order_Line_Unit_Sale_Price,Order_Amount,
         Order_Customer_ID,Product,Order_Credit_Card_Payment_Amount,BrandName,Order_Hour_of_Day,Order_Day_of_Week,
         Order_Credit_Card_Brand,Gender) %>%
  group_by(Order_Customer_ID,Order_ID,Order_Line_ID)


buffer_order <- order_df %>%
  select(Order_Customer_ID,Order_ID)%>%
  group_by(Order_Customer_ID) %>%
  summarise(Orders = length(unique(Order_ID)))


buffer <- merge(buffer,buffer_order,by = "Order_Customer_ID")


buffer <- buffer %>%
  select(Order_Customer_ID,Order_ID,Order_Line_ID,Order_Line_Quantity,Product_ID,Order_Date,Order_Promotion_Code,Order_Discount_Amount,Order_Line_Unit_List_Price,
         Order_Line_Unit_Sale_Price,Order_Amount,Product,Order_Credit_Card_Payment_Amount,BrandName,Order_Hour_of_Day,Order_Day_of_Week,
         Order_Credit_Card_Brand,Gender,Orders)%>%
  group_by(Order_ID)%>%
  summarise(Order_Line_Quantity = first(Order_Line_Quantity),
            Product_ID = first(Product_ID),
            Order_Date = first(Order_Date),
            Order_Promotion_Code = first(Order_Promotion_Code),
            Order_Discount_Amount = first(Order_Discount_Amount),
            Order_Line_Unit_List_Price = first(Order_Line_Unit_List_Price),
            Order_Line_Unit_Sale_Price = first(Order_Line_Unit_Sale_Price),
            Order_Amount = first(Order_Amount),
            Product = first(Product),
            Order_Credit_Card_Payment_Amount = first(Order_Credit_Card_Payment_Amount),
            BrandName = first(BrandName),
            Order_Hour_of_Day = first(Order_Hour_of_Day),
            Order_Day_of_Week = first(Order_Day_of_Week),
            Order_Credit_Card_Brand = first(Order_Credit_Card_Brand),
            Gender= first(Gender),
            Orders = first(Orders))

buffer$moretimebuyer <- c(TRUE)

buffer$moretimebuyer <- replace(buffer$moretimebuyer,buffer$Orders< 2,FALSE)

buffer <-buffer[order(-buffer$moretimebuyer),]


buffer <- buffer %>%
  select(Order_Line_Quantity,Product_ID,Order_Date,Order_Promotion_Code,
         Order_Discount_Amount,Order_Line_Unit_List_Price,Order_Line_Unit_Sale_Price,Order_Amount,
         Product,Order_Credit_Card_Payment_Amount,BrandName,Order_Hour_of_Day,Order_Day_of_Week,
         Order_Credit_Card_Brand,Gender,moretimebuyer)


#split T and F and randomize

buffer3 <- filter(buffer,moretimebuyer==TRUE)

buffer3 <- buffer3[sample(nrow(buffer3)),]

buffer4 <- filter(buffer,moretimebuyer==FALSE)

buffer4 <- buffer4[sample(nrow(buffer4)),]

train <- buffer3[1:78,]
train <- rbind(train,buffer4[1:100,])

test <- buffer3[78:156,]
test <- rbind(test,buffer4[100:200,])



library(rpart)

fit <- rpart(moretimebuyer~
               Order_Credit_Card_Payment_Amount+Order_Hour_of_Day+Order_Day_of_Week+Order_Credit_Card_Brand+
               Gender
             ,data = train,method = "class")


plot(fit, uniform = TRUE,margin = 0.1,main = "O Tree")
text(fit, use.n = TRUE,all = TRUE,cex=.8)
prediction <- predict(fit,test,type = "class")
table(prediction,test$moretimebuyer) 

plotcp(fit)
kable(fit$cptable)


# cp 0.06 best performace for minimal False classified as True

# cp 0.04 to 0.56 best performace for minimal True classified as False

fit.pruned <- prune(fit,cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"] )


plot(fit.pruned, uniform = TRUE,margin = 0.1, main= "P Tree")
text(fit.pruned, use.n = TRUE,all = TRUE,cex=.8)
prediction_p <- predict(fit.pruned,test,type = "class")
table(prediction_p,test$moretimebuyer) 




