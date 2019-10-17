library(dplyr)
library(ggplot2)
library(ineq)
library(stringr)
library(boot)

orders_raw <- read.csv("data/interim/orders/orders_with_headers.csv")
orders_raw$Brand = unlist(lapply(orders_raw$Product_Level_2_Path, FUN = function(path){return(str_match(path, "\\/.*\\/.*\\/(.*)")[,2])}))

orders <- orders_raw %>%
  distinct(Customer_ID, .keep_all = TRUE) %>%
  filter(as.character(Gender) != "NULL") %>%
  filter(as.character(Age) != "?") %>%
  mutate(Age = as.numeric(as.character(Age))) %>%
  droplevels(exclude=c("NULL"))

# density plot
ggplot(orders, mapping = aes(Age, linetype=Gender)) +
  stat_density(geom="line", size =1.25 )

# lorenz curves
## total spending per customer
money_spent_per_customer <- orders_raw %>%
  group_by(Customer_ID) %>%
  summarise(Total_Spending = sum(Order_Line_Amount))
  
lorenz_money_spent_per_customer <- Lc(money_spent_per_customer$Total_Spending)
ineq(money_spent_per_customer$Total_Spending)
upper_ten_percent_customer <- 1- lorenz_money_spent_per_customer$L[nrow(money_spent_per_customer)*0.9]
plot(lorenz_money_spent_per_customer)

## total spending per brand
money_spent_per_brand <- orders_raw %>%
  group_by(Brand) %>%
  summarise(Total_Spending = mean(Order_Line_Amount))

lorenz_money_spent_per_brand <- Lc(money_spent_per_brand$Total_Spending)
ineq(money_spent_per_brand$Total_Spending)
upper_ten_percent_brand <- 1 - lorenz_money_spent_per_brand$L[nrow(money_spent_per_brand)*0.9]
plot(lorenz_money_spent_per_brand)
  
# Experiment
data <- read.csv("experiment/experimental_results.csv")

print("Create rearranged dataframe of experimental_results.csv...")
# build new dataframe with one row per customer
recommender_to_sales <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Id", "Recommender", "Sales"))
index <- 1
for(row in 1:nrow(data)){
  if(!is.na(data[row, "ranking_based"])){
    recommender_to_sales[nrow(recommender_to_sales) +1,] = list(index, "ranking_based", as.numeric(data[row, "ranking_based"]))
    index <- index+1
  }
  if(!is.na(data[row, "random_recommendations"])){
    recommender_to_sales[nrow(recommender_to_sales) +1,] = list(index, "random_recommendations", as.numeric(data[row, "random_recommendations"]))
    index <- index+1
  }
  if(!is.na(data[row, "profit_oriented"])){
    recommender_to_sales[nrow(recommender_to_sales) +1,] = list(index, "profit_oriented", as.numeric(data[row, "profit_oriented"]))
    index <- index+1
  }
}
recommender_to_sales$Id <- NULL
print("Done.")

# visualize distribution
ggplot(recommender_to_sales %>% filter(Recommender=="ranking_based"), aes(x=Sales)) +
  stat_density(geom="line") +
  geom_vline(aes(xintercept=mean(Sales)),color="black", linetype="dashed", size=1)

# visualize distribution
ggplot(recommender_to_sales %>% filter(Recommender=="profit_oriented"), aes(x=Sales)) +
  stat_density(geom="line") +
  geom_vline(aes(xintercept=mean(Sales)),color="black", linetype="dashed", size=1)

# visualize distribution
ggplot(recommender_to_sales %>% filter(Recommender=="random_recommendations"), aes(x=Sales)) +
  stat_density(geom="line") +
  geom_vline(aes(xintercept=mean(Sales)),color="black", linetype="dashed", size=1)


result <- group_by(recommender_to_sales, Recommender) %>%
  summarise(
    Count = n(),
    Mean = mean(Sales, na.rm = TRUE),
    Sd = sd(Sales, na.rm = TRUE),
    Se = Sd/sqrt(Count),
    CiMult = qt(0.975, Count-1),
    Ci = Se * CiMult
  )

# visualize CI
ggplot(result, aes(x=Recommender, y=Mean, group=1)) +
  geom_point(alpha=0.52) +
  geom_errorbar(width=.1, aes(ymin=Mean-Ci, ymax=Mean+Ci), colour="darkred") + labs(x="Recommender",y= "Sales", title="Recommender Confidence Intervals")



# not normally distributed
shapiro_wilk_profit <- with(recommender_to_sales, shapiro.test(Sales[Recommender == "profit_oriented"])) 
shapiro_wilk_ranking <- with(recommender_to_sales, shapiro.test(Sales[Recommender == "ranking_based"]))

# equality of variances
recommender_to_sales_without_random <- recommender_to_sales %>%
  filter(!Recommender == "random_recommendations")
res.ftest <- var.test(Sales ~ Recommender, data = recommender_to_sales_without_random)

# perform wulxoc test
ranking_based_sales <- recommender_to_sales %>% filter(Recommender == "ranking_based")
profit_oriented_sales <- recommender_to_sales %>% filter(Recommender == "profit_oriented")
random_recommendations_sales <- recommender_to_sales %>% filter(Recommender == "random_recommendations")

wilcox.test(profit_oriented_sales$Sales, random_recommendations_sales$Sales, alternative = "two.sided")
wilcox.test(ranking_based_sales$Sales, random_recommendations_sales$Sales, alternative = "two.sided")
wilcox.test(profit_oriented_sales$Sales, random_recommendations_sales$Sales, alternative = "two.sided")