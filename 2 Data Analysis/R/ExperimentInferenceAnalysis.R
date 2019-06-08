library("infer")
library("dplyr")

pathExperiment <- "experiment/experimental_results.csv"
experiment <- read.csv(file=pathExperiment)

# Reformat the results to allow inference analysis
experimentDf <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("Sales_in_EUR","Used_Profit_Oriented_Reccomendations","Used_Top_Reccomendations")
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

experimentDf[,"Used_Profit_Oriented_Reccomendations"] <- factor(experimentDf[,"Used_Profit_Oriented_Reccomendations"])
experimentDf[,"Used_Top_Reccomendations"] <- factor(experimentDf[,"Used_Top_Reccomendations"])
experiment <- experimentDf

# Do inference analysis
# Part 1: Test if the profit oriented reccomendation system causes a significant difference in sales

obs_stat <- experiment %>% 
  filter(Used_Top_Reccomendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Profit_Oriented_Reccomendations) %>%
  calculate(stat = "diff in means", order=c("1","0"))

permuted_stat <- experiment %>% 
  filter(Used_Top_Reccomendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Profit_Oriented_Reccomendations) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type="permute") %>%
  calculate(stat = "diff in means", order=c("1","0"))

p_val <- permuted_stat %>%
  get_p_value(obs_stat = obs_stat, direction = "two_sided")

viz_plot <- permuted_stat %>% visualize()
viz_plot <- viz_plot + shade_p_value(obs_stat, direction = "two_sided", color = "black", fill = "grey")
print(viz_plot)

bootstrap_stat <- experiment %>%
  filter(Used_Top_Reccomendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Profit_Oriented_Reccomendations) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("1", "0"))

ci <- bootstrap_stat %>%
  get_confidence_interval(level = 0.9)

viz_plot <- bootstrap_stat[complete.cases(bootstrap_stat), ] %>% visualize()
viz_plot <- viz_plot + shade_confidence_interval(ci, color = "black", fill = "grey")
print(viz_plot)

# Part 2: Test if the popular products reccomendation system causes a significant difference in sales

obs_stat <- experiment %>% 
  filter(Used_Profit_Oriented_Reccomendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Top_Reccomendations) %>%
  calculate(stat = "diff in means", order=c("1","0"))

permuted_stat <- experiment %>% 
  filter(Used_Profit_Oriented_Reccomendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Top_Reccomendations) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type="permute") %>%
  calculate(stat = "diff in means", order=c("1","0"))

p_val <- permuted_stat %>%
  get_p_value(obs_stat = obs_stat, direction = "two_sided")

viz_plot <- permuted_stat %>% visualize()
viz_plot <- viz_plot + shade_p_value(obs_stat, direction = "two_sided", color = "black", fill = "grey")
print(viz_plot)

bootstrap_stat <- experiment %>%
  filter(Used_Profit_Oriented_Reccomendations == 0) %>%
  specify(Sales_in_EUR ~ Used_Top_Reccomendations) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("1", "0"))

ci <- bootstrap_stat %>%
  get_confidence_interval(level = 0.9)

viz_plot <- bootstrap_stat[complete.cases(bootstrap_stat), ] %>% visualize()
viz_plot <- viz_plot + shade_confidence_interval(ci, color = "black", fill = "grey")
print(viz_plot)