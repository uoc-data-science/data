library(tidyverse)
library(stats)

df <- read.csv("experiment/experimental_results.csv")

df_clean <- df %>%
  select(id, ranking_based, random_recommendations, profit_oriented) %>%
  gather("recommender", "value", 2:4, na.rm = TRUE)

df_base <- df_clean %>%
  filter(recommender == "random_recommendations")

with(data = df_clean, t.test(x = value[recommender == "ranking_based"], y = value[recommender == "random_recommendations"]))

with(data = df_clean, t.test(x = value[recommender == "profit_oriented"], y = value[recommender == "random_recommendations"]))

with(data = df_clean, t.test(x = value[recommender == "ranking_based"], y = value[recommender == "profit_oriented"]))
