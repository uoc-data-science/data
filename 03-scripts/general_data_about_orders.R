library(dplyr)
library(sqldf)
library(stringr)
library(knitr)
library(kableExtra)

cleanorders <- read.csv("02-clean-data/orders/order_data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
n_orders = nrow(cleanorders)




unique_checkouts <- distinct(cleanorders, Order.ID, .keep_all = TRUE)

paid_money <- select(unique_checkouts, "Age", "Gender", "Presence.Of.Children", "Order.Amount", "Estimated.Income.Code", "Order.Customer.ID")

paid_money <- filter(paid_money, paid_money$Order.Amount != "0.00")

# Avg. order amount per gender

paid_by_female <- filter(paid_money, paid_money$Gender == "Female")
paid_by_male <- filter(paid_money, paid_money$Gender == "Male")
paid_by_both <- rbind(paid_by_female, paid_by_male)
avg_cart_female <- mean(paid_by_female$Order.Amount)
avg_cart_male <- mean(paid_by_male$Order.Amount)
avg_cart_both <- mean(paid_by_both$Order.Amount)

# Avg. order amount per gender and income

paid_per_income <- filter(paid_money, paid_money$Estimated.Income.Code != "?" & paid_money$Gender != "NULL")
for (i in 1:nrow(paid_per_income)) {
  if(paid_per_income[i,5] == "Under $15;000")paid_per_income[i,5] = "0"
  else if(paid_per_income[i,5] == "$15;000-$19;999")paid_per_income[i,5] = "15000"
  else if(paid_per_income[i,5] == "$20;000-$29;999")paid_per_income[i,5] = "20000"
  else if(paid_per_income[i,5] == "$30;000-$39;999")paid_per_income[i,5] = "30000"
  else if(paid_per_income[i,5] == "$40;000-$49;999")paid_per_income[i,5] = "40000"
  else if(paid_per_income[i,5] == "$50;000-$74;999")paid_per_income[i,5] = "50000"
  else if(paid_per_income[i,5] == "$75;000-$99;999")paid_per_income[i,5] = "75000"
  else if(paid_per_income[i,5] == "$100;000-$124;999")paid_per_income[i,5] = "100000"
  else if(paid_per_income[i,5] == "$125;000 OR MORE")paid_per_income[i,5] = "125000"
}

n_paid_per_income <- nrow(paid_per_income)

n_b1 <- nrow(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "0"))
r_b1 <- round((n_b1/n_paid_per_income)*100, digits = 2)
n_b2 <- nrow(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "15000"))
r_b2 <- round((n_b2/n_paid_per_income)*100, digits = 2)
n_b3 <- nrow(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "20000"))
r_b3 <- round((n_b3/n_paid_per_income)*100, digits = 2)
n_b4 <- nrow(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "30000"))
r_b4 <- round((n_b4/n_paid_per_income)*100, digits = 2)
n_b5 <- nrow(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "40000"))
r_b5 <- round((n_b5/n_paid_per_income)*100, digits = 2)
n_b6 <- nrow(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "50000"))
r_b6 <- round((n_b6/n_paid_per_income)*100, digits = 2)
n_b7 <- nrow(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "75000"))
r_b7 <- round((n_b7/n_paid_per_income)*100, digits = 2)
n_b8 <- nrow(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "100000"))
r_b8 <- round((n_b8/n_paid_per_income)*100, digits = 2)
n_b9 <- nrow(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "125000"))
r_b9 <- round((n_b9/n_paid_per_income)*100, digits = 2)

m1 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "0" & paid_per_income$Gender == "Male")$Order.Amount)), digits = 2)
m2 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "15000" & paid_per_income$Gender == "Male")$Order.Amount)), digits = 2)
m3 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "20000" & paid_per_income$Gender == "Male")$Order.Amount)), digits = 2)
m4 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "30000" & paid_per_income$Gender == "Male")$Order.Amount)), digits = 2)
m5 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "40000" & paid_per_income$Gender == "Male")$Order.Amount)), digits = 2)
m6 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "50000" & paid_per_income$Gender == "Male")$Order.Amount)), digits = 2)
m7 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "75000" & paid_per_income$Gender == "Male")$Order.Amount)), digits = 2)
m8 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "100000" & paid_per_income$Gender == "Male")$Order.Amount)), digits = 2)
m9 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "125000" & paid_per_income$Gender == "Male")$Order.Amount)), digits = 2)

f1 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "0" & paid_per_income$Gender == "Female")$Order.Amount)), digits = 2)
f2 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "15000" & paid_per_income$Gender == "Female")$Order.Amount)), digits = 2)
f3 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "20000" & paid_per_income$Gender == "Female")$Order.Amount)), digits = 2)
f4 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "30000" & paid_per_income$Gender == "Female")$Order.Amount)), digits = 2)
f5 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "40000" & paid_per_income$Gender == "Female")$Order.Amount)), digits = 2)
f6 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "50000" & paid_per_income$Gender == "Female")$Order.Amount)), digits = 2)
f7 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "75000" & paid_per_income$Gender == "Female")$Order.Amount)), digits = 2)
f8 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "100000" & paid_per_income$Gender == "Female")$Order.Amount)), digits = 2)
f9 <- round((mean(filter(paid_per_income, paid_per_income$Estimated.Income.Code == "125000" & paid_per_income$Gender == "Female")$Order.Amount)), digits = 2)

b1 <- (m1+f1)/2
b2 <- (m2+f2)/2
b3 <- (m3+f3)/2
b4 <- (m4+f4)/2
b5 <- (m5+f5)/2
b6 <- (m6+f6)/2
b7 <- (m7+f7)/2
b8 <- (m8+f8)/2
b9 <- (m9+f9)/2

avg_order_amount_income_table <- data.frame("Income at least" = c("0","15000","20000","30000","40000","50000","75000","100000","125000"))
avg_order_amount_income_table <- cbind(avg_order_amount_income_table, "Ratio (%)" = c(r_b1, r_b2, r_b3, r_b4, r_b5, r_b6, r_b7, r_b8, r_b9))
avg_order_amount_income_table <- cbind(avg_order_amount_income_table, "Male (USD)" = c(m1, m2, m3, m4, m5, m6, m7, m8, m9))
avg_order_amount_income_table <- cbind(avg_order_amount_income_table, "Female (USD)" = c(f1, f2, f3, f4, f5, f6, f7, f8, f9))
avg_order_amount_income_table <- cbind(avg_order_amount_income_table, "Both (USD)" = c(b1, b2, b3, b4, b5, b6, b7, b8, b9))

avg_income_all <- (r_b1*0+r_b2*15000+r_b3*20000+r_b4*30000+r_b5*40000+r_b6*50000+r_b7*75000+r_b8*100000+r_b9*125000) 
avg_order_men <- (r_b1*m1+r_b2*m2+r_b3*m3+r_b4*m4+r_b5*m5+r_b6*m6+r_b7*m7+r_b8*m8+r_b9*m9)
avg_order_female <- (r_b1*f1+r_b2*f2+r_b3*f3+r_b4*f4+r_b5*f5+r_b6*f6+r_b7*f7+r_b8*f8+r_b9*f9)
avg_order_amount_income_table <- cbind(avg_order_amount_income_table, "Both (USD)" = c(b1, b2, b3, b4, b5, b6, b7, b8, b9))

colnames(avg_order_amount_income_table) <- c("Minimum income in USD", "Ratio (%)", "Male (USD)", "Female (USD)", "Both (USD)")


# Top categories and brands

total_purchases <- select(cleanorders, "Manufacturer", "Audience")
n_total_purchases <- nrow(total_purchases)

for (i in 1:nrow(total_purchases)) {
  if(total_purchases[i,2] == "NULL")total_purchases[i,2] = "No Category"
}

ratio_audience <- count(total_purchases$'Audience')
colnames(ratio_audience) <- c("Category", "Amount")
ratio_audience <- ratio_audience[order(ratio_audience$Amount, decreasing = TRUE),]
ratio_audience_table <- select(ratio_audience, "Category", "Amount")



rat1 <- as.numeric(round(ratio_audience_table[1,2]/n_total_purchases*100, digits = 2))
rat2 <- as.numeric(round(ratio_audience_table[2,2]/n_total_purchases*100, digits = 2))
rat3 <- as.numeric(round(ratio_audience_table[3,2]/n_total_purchases*100, digits = 2))
rat4 <- as.numeric(round(ratio_audience_table[4,2]/n_total_purchases*100, digits = 2))

ratio_audience_table <- cbind(ratio_audience_table, "Ratio" = c(rat1,rat2,rat3,rat4))

ratio_brands <- filter(total_purchases, total_purchases$'Manufacturer' != "?", total_purchases$Audience != "NULL")
ratio_brands_all <- count(ratio_brands$'Manufacturer')
colnames(ratio_brands_all) <- c("Brand", "Overall")
ratio_brands_all <- ratio_brands_all[order(ratio_brands_all$'Overall', decreasing = TRUE), ]

ratio_brands_women <- filter(ratio_brands, ratio_brands$'Audience' == "Women")
ratio_brands_women <- count(ratio_brands_women$'Manufacturer')
colnames(ratio_brands_women) <- c("Brand", "Women")

ratio_brands_men <- filter(ratio_brands, ratio_brands$'Audience' == "Men")
ratio_brands_men <- count(ratio_brands_men$'Manufacturer')
colnames(ratio_brands_men) <- c("Brand", "Men")

ratio_brands_children <- filter(ratio_brands, ratio_brands$'Audience' == "Children")
ratio_brands_children <- count(ratio_brands_children$'Manufacturer')
colnames(ratio_brands_children) <- c("Brand", "Children")

ratio_brands_table <- left_join(ratio_brands_all, ratio_brands_women, by ="Brand", match = "")
ratio_brands_table <- left_join(ratio_brands_table, ratio_brands_men, by ="Brand", match = "")
ratio_brands_table <- left_join(ratio_brands_table, ratio_brands_children, by ="Brand", match = "")
ratio_brands_table[is.na(ratio_brands_table)] <- 0

sum_overall <- sum(as.numeric(ratio_brands_table$'Overall'), na.rm = TRUE)
sum_women <- sum(as.numeric(ratio_brands_table$'Women'), na.rm = TRUE)
sum_men <- sum(as.numeric(ratio_brands_table$'Men'), na.rm = TRUE)
sum_children <- sum(as.numeric(ratio_brands_table$'Children'), na.rm = TRUE)

n_brands_total <- nrow(ratio_brands_table)
ratio_brands_table <- ratio_brands_table[1:10,]

sum_overall_top10 <- sum(as.numeric(ratio_brands_table$'Overall'), na.rm = TRUE)
sum_women_top10 <- sum(as.numeric(ratio_brands_table$'Women'), na.rm = TRUE)
sum_men_top10 <- sum(as.numeric(ratio_brands_table$'Men'), na.rm = TRUE)
sum_children_top10 <- sum(as.numeric(ratio_brands_table$'Children'), na.rm = TRUE)

ratio_brands_table <- rbind(ratio_brands_table, c("All other brands:",(sum_women-sum_women_top10+sum_men-sum_men_top10+sum_children-sum_children_top10),sum_women-sum_women_top10,sum_men-sum_men_top10,sum_children-sum_children_top10))

percentage_other_brands <- 100*as.numeric(ratio_brands_table$Overall[11])/sum(as.numeric(ratio_brands_table$Overall[1:10]))

usage_of_coupons <- select(unique_checkouts, "Order.Amount", "Order.Promotion.Code", "Gender", "Order.Discount.Amount")
usage_of_coupons <- filter(usage_of_coupons, usage_of_coupons$'Gender' == "Male" | usage_of_coupons$'Gender' == "Female")
uoc_male <- filter(usage_of_coupons, usage_of_coupons$'Gender' == "Male")
uoc_female <- filter(usage_of_coupons, usage_of_coupons$'Gender' == "Female")
n_usage_of_coupons <- nrow(uoc_male)+nrow(uoc_female)

ratio_coupons_table <- data.frame("Gender" = c("Overall", "Male", "Female"))
avg_coupon_value <- mean(as.numeric(filter(usage_of_coupons, usage_of_coupons$'Order.Discount.Amount' != "0.00")$'Order.Discount.Amount'))
avg_coupon_value <- round(avg_coupon_value, digits = 2)

usage_male <- filter(uoc_male, uoc_male$'Order.Promotion.Code' != "NULL")
usage_female <- filter(uoc_female, uoc_female$'Order.Promotion.Code' != "NULL")
usage_all <- (nrow(usage_male)+nrow(usage_female))/(nrow(uoc_male)+nrow(uoc_female))

ratio_coupons_table <- cbind(ratio_coupons_table, "Usage" = c(round(usage_all*100, digits = 2), round(nrow(usage_male)/nrow(uoc_male)*100, digits = 2), round(nrow(usage_female)/nrow(uoc_female)*100, digits = 2)))



coupon_value <- sum(as.numeric(filter(usage_of_coupons, usage_of_coupons$'Order.Promotion.Code' != "NULL")$'Order.Discount.Amount'))/nrow(filter(usage_of_coupons, usage_of_coupons$'Order.Promotion.Code' != "NULL"))
avg_cart_with_male <- round(sum(as.numeric(filter(uoc_male, uoc_male$'Order.Promotion.Code' != "NULL")$'Order.Discount.Amount')+as.numeric(filter(uoc_male, uoc_male$'Order.Promotion.Code' != "NULL")$'Order.Amount'), na.rm = TRUE)/nrow(filter(uoc_male, uoc_male$'Order.Promotion.Code' != "NULL")), digits = 2)
avg_cart_with_female <- round(sum(as.numeric(filter(uoc_female, uoc_female$'Order.Promotion.Code' != "NULL")$'Order.Discount.Amount')+as.numeric(filter(uoc_female, uoc_female$'Order.Promotion.Code' != "NULL")$'Order.Amount'), na.rm = TRUE)/nrow(filter(uoc_female, uoc_female$'Order.Promotion.Code' != "NULL")), digits = 2)
avg_cart_with_all <- round(sum(as.numeric(filter(usage_of_coupons, usage_of_coupons$'Order.Promotion.Code' != "NULL")$'Order.Discount.Amount')+as.numeric(filter(usage_of_coupons, usage_of_coupons$'Order.Promotion.Code' != "NULL")$'Order.Amount'), na.rm = TRUE)/nrow(filter(usage_of_coupons, usage_of_coupons$'Order.Promotion.Code' != "NULL")), digits = 2)
avg_cart_without_male <- round(sum(as.numeric(filter(uoc_male, uoc_male$'Order.Promotion.Code' == "NULL")$'Order.Amount'), na.rm = TRUE)/nrow(filter(uoc_male, uoc_male$'Order.Promotion.Code' == "NULL")), digits = 2)
avg_cart_without_female <- round(sum(as.numeric(filter(uoc_female, uoc_female$'Order.Promotion.Code' == "NULL")$'Order.Amount'), na.rm = TRUE)/nrow(filter(uoc_female, uoc_female$'Order.Promotion.Code' == "NULL")), digits = 2)
avg_cart_without_all <- round(sum(as.numeric(filter(usage_of_coupons, usage_of_coupons$'Order.Promotion.Code' == "NULL")$'Order.Amount'), na.rm = TRUE)/nrow(filter(usage_of_coupons, usage_of_coupons$'Order.Promotion.Code' == "NULL")), digits = 2)

ratio_coupons_table <- cbind(ratio_coupons_table, "WITHOUT coupon" = c(avg_cart_without_all, avg_cart_without_male, avg_cart_without_female))
ratio_coupons_table <- cbind(ratio_coupons_table, "WITH coupon" = c(avg_cart_with_all, avg_cart_with_male, avg_cart_with_female))
