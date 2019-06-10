library("infer")
library("dplyr")

pathExperiment <- "experiment/experimental_results.csv"
experiment <- read.csv(file=pathExperiment)

# Reformat the results to allow inference analysis
experimentDf <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("Sales_in_EUR","Used_Profit_Oriented_recommendations","Used_Top_recommendations")
colnames(experimentDf) <- x

for (row in 1:nrow(experiment)){
  random <- experiment[row, "random_recommendations"]
  profit <- experiment[row, "profit_oriented"]
  top <- experiment[row, "ranking_based"]
  if (!is.na(random)){
    experimentDf[nrow(experimentDf) + 1,] = list(random,0,0)
  }
  if (!is.na(profit)){
    experimentDf[nrow(experimentDf) + 1,] = list(profit,1,0)
  }
  if (!is.na(top)){
    experimentDf[nrow(experimentDf) + 1,] = list(top,0,1)
  }
}

experimentDf[,"Used_Profit_Oriented_recommendations"] <- factor(experimentDf[,"Used_Profit_Oriented_recommendations"])
experimentDf[,"Used_Top_recommendations"] <- factor(experimentDf[,"Used_Top_recommendations"])
experiment <- experimentDf

# Do inference analysis
# Part 1: Test if the profit oriented recommendation system causes a significant difference in sales

obs_stat <- experiment %>% 
  filter(Used_Top_recommendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Profit_Oriented_recommendations) %>%
  calculate(stat = "diff in means", order=c("1","0"))

permuted_stat <- experiment %>% 
  filter(Used_Top_recommendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Profit_Oriented_recommendations) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type="permute") %>%
  calculate(stat = "diff in means", order=c("1","0"))

p_val <- permuted_stat %>%
  get_p_value(obs_stat = obs_stat, direction = "two_sided")
print(paste("p-value = ",as.data.frame(p_val)[1,1],sep=""))

viz_plot <- permuted_stat %>% visualize()
viz_plot <- viz_plot + shade_p_value(obs_stat, direction = "two_sided", color = "black", fill = "grey")
print(viz_plot)

bootstrap_stat <- experiment %>%
  filter(Used_Top_recommendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Profit_Oriented_recommendations) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("1", "0"))

# Bonferroni correction: Default alpha = 5%, number of tests = 2 -> Set alpha to 2.5% -> level = 0.95
ci <- bootstrap_stat %>%
  get_confidence_interval(level = 0.95)
print(ci)

viz_plot <- bootstrap_stat[complete.cases(bootstrap_stat), ] %>% visualize()
viz_plot <- viz_plot + shade_confidence_interval(ci, color = "black", fill = "grey")
print(viz_plot)

# Part 2: Test if the popular products recommendation system causes a significant difference in sales

obs_stat <- experiment %>% 
  filter(Used_Profit_Oriented_recommendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Top_recommendations) %>%
  calculate(stat = "diff in means", order=c("1","0"))

permuted_stat <- experiment %>% 
  filter(Used_Profit_Oriented_recommendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Top_recommendations) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type="permute") %>%
  calculate(stat = "diff in means", order=c("1","0"))

p_val <- permuted_stat %>%
  get_p_value(obs_stat = obs_stat, direction = "two_sided")
print(paste("p-value = ",as.data.frame(p_val)[1,1],sep=""))

viz_plot <- permuted_stat %>% visualize()
viz_plot <- viz_plot + shade_p_value(obs_stat, direction = "two_sided", color = "black", fill = "grey")
print(viz_plot)

bootstrap_stat <- experiment %>%
  filter(Used_Profit_Oriented_recommendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Top_recommendations) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("1", "0"))

# Bonferroni correction: Default alpha = 5%, number of tests = 2 -> Set alpha to 2.5% -> level = 0.95
ci <- bootstrap_stat %>%
  get_confidence_interval(level = 0.95)
print(ci)

viz_plot <- bootstrap_stat[complete.cases(bootstrap_stat), ] %>% visualize()
viz_plot <- viz_plot + shade_confidence_interval(ci, color = "black", fill = "grey")
print(viz_plot)