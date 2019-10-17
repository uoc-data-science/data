library(dplyr)
library(sqldf)
library(knitr)
library(kableExtra)

cleanorders <- read.csv("02-clean-data/orders/order_data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
n_orders = nrow(cleanorders)

# Diminish repeatedly contained customers

unique_customers <- distinct(cleanorders, Order.Customer.ID, .keep_all = TRUE)

age <- select(unique_customers, "Age", "Gender", "Order.Customer.ID")
age <- unique(age, by = "Order.Customer.ID")
age <- filter(age, age$'Age' != "?", age$'Age' != "NULL", age$'Gender' != "NULL", age$'Gender' != "?")
n_nonduplicated <- nrow(age)

# Age of customers

age_all <- round(mean(as.numeric(age$'Age')), digits = 0)
age_men <- filter(age, age$'Gender' == "Male")
age_women <- filter(age, age$'Gender' == "Female")
avg_age_all <- age_all
avg_age_men <- round(mean(as.numeric(age_men$'Age')), digits = 0)
avg_age_women <- round(mean(as.numeric(age_women$'Age')), digits = 0)
min_age_men <- min(as.numeric(age_men$'Age'))
min_age_women <- min(as.numeric(age_women$'Age'))
max_age_men <- max(as.numeric(age_men$'Age'))
max_age_women <- max(as.numeric(age_women$'Age'))
std_age_men <- sd(as.numeric(age_men$'Age'))
std_age_women <- sd(as.numeric(age_women$'Age'))
std_age_all <- sd(as.numeric(age$'Age'))

# male/female Ratio of customers 
ratio_women <- round(nrow(age_women)/n_nonduplicated*100, digits = 2)
ratio_men <- round(nrow(age_men)/n_nonduplicated*100, digits = 2)

# Income of customers

income <- select(unique_customers, "Gender", "Estimated.Income.Code")
income <- filter(income, income$'Gender' != "NULL", income$'Gender' != "?", income$'Estimated.Income.Code' !="?")
income_men <- filter(income, income$'Gender' == "Male")
income_women <- filter(income, income$'Gender' == "Female")

income_all <- count(income$Estimated.Income.Code)
income_men <- count(income_men$Estimated.Income.Code)
income_women <- count(income_women$Estimated.Income.Code)

colnames(income_all) <- c("Amount", "income_all")
colnames(income_men) <- c("Amount", "income_men")
colnames(income_women) <- c("Amount", "income_women")

income <- join(income_all, income_men, by = "Amount", type = "left", match = "all")
income <- join(income, income_women, by = "Amount", type = "left", match = "all")

for (i in 1:nrow(income)) {
  if(income[i,1] == "Under $15;000")income[i,1] = "0"
  else if(income[i,1] == "$15;000-$19;999")income[i,1] = "15000"
  else if(income[i,1] == "$20;000-$29;999")income[i,1] = "20000"
  else if(income[i,1] == "$30;000-$39;999")income[i,1] = "30000"
  else if(income[i,1] == "$40;000-$49;999")income[i,1] = "40000"
  else if(income[i,1] == "$50;000-$74;999")income[i,1] = "50000"
  else if(income[i,1] == "$75;000-$99;999")income[i,1] = "75000"
  else if(income[i,1] == "$100;000-$124;999")income[i,1] = "100000"
  else if(income[i,1] == "$125;000 OR MORE")income[i,1] = "125000"
}

avg_income_all <- round((sum(as.numeric(income$Amount)*as.numeric(income$income_all)))/(sum(income$income_all)))
avg_income_men <- round((sum(as.numeric(income$Amount)*as.numeric(income$income_men)))/(sum(income$income_men)))
avg_income_women <- round((sum(as.numeric(income$Amount)*as.numeric(income$income_women)))/(sum(income$income_women)))

# Family status

famstatus <- select(unique_customers, "Gender", "Marital.Status", "Presence.Of.Children")
famstatus <- filter(famstatus, famstatus$Gender != "NULL", famstatus$Marital.Status != "NULL")
n_famstatus <- nrow(famstatus)

fam_female <- filter(famstatus, famstatus$Gender == "Female")
fam_male <- filter(famstatus, famstatus$Gender == "Male")
fam_female_marr <- filter(fam_female, fam_female$Marital.Status == "Married" | fam_female$Marital.Status == "Inferred Married")
fam_male_marr <- filter(fam_male, fam_male$Marital.Status != "Single", fam_male$Marital.Status != "Inferred Single")

fam_stat_table <- data.frame("Gender" = c("All", "Female", "Male"))
fam_ratio_male <- round((nrow(fam_male)/n_famstatus*100), digits = 2)
fam_ratio_female <- round((nrow(fam_female)/n_famstatus*100), digits = 2)
fam_stat_table <- cbind(fam_stat_table, "Ratio" = c("100", fam_ratio_female, fam_ratio_male))

fam_stat_table <- cbind(fam_stat_table, "Average Age" = c(avg_age_all, avg_age_women, avg_age_men))
fam_stat_table <- cbind(fam_stat_table, "Age Std. Dev." = c(std_age_all, std_age_women, std_age_men))

fam_married_male <- round((nrow(fam_male_marr)/nrow(fam_male)*100), digits = 2)
fam_married_female <- round((nrow(fam_female_marr)/nrow(fam_female)*100), digits = 2)
fam_married_all <- round(((nrow(fam_male_marr)+nrow(fam_female_marr))/n_famstatus*100), digits = 2)
fam_stat_table <- cbind(fam_stat_table, "Married" = c(fam_married_all, fam_married_female, fam_married_male))

fam_female_child <- filter(fam_female, fam_female$Presence.Of.Children == "True")
fam_male_child <- filter(fam_male, fam_male$Presence.Of.Children == "True")
fam_all_child <- round((nrow(fam_female_child)+nrow(fam_male_child))/n_famstatus*100, digits = 2)
fam_stat_table <- cbind(fam_stat_table, "Parent" = c(fam_all_child, round(nrow(fam_female_child)/nrow(fam_female)*100, digits = 2), round(nrow(fam_male_child)/nrow(fam_male)*100, digits = 2)))

colnames(fam_stat_table) <- c("Gender", "Ratio (%)", "Average Age (yr)", "Std. Dev. Age", "Married (%)", "Parent (%)")