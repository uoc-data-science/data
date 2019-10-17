# Sources:
# https://www.rstudio.com/resources/videos/infer-a-package-for-tidy-statistical-inference/
# https://infer.netlify.com/reference/index.html

library("infer")
library("dplyr")
mtcars <- as.data.frame(mtcars) %>%
  mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))

# Example where the response variable is a factor -> Chi squared statistic
# Analyze dependence of gear on am

obs_stat <- mtcars %>% 
  specify(gear ~ am) %>%
  calculate(stat = "Chisq")

permuted_stat <- mtcars %>% 
  specify(gear ~ am) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type="permute") %>%
  calculate(stat = "Chisq") 

p_val <- permuted_stat %>%
  get_p_value(obs_stat = obs_stat, direction = "right")

viz_plot <- permuted_stat %>%
  visualize()
viz_plot <- viz_plot + shade_p_value(obs_stat, direction = "right")
print(viz_plot)

bootstrap_stat <- mtcars %>%
  specify(gear ~ am) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "Chisq")

ci <- bootstrap_stat[complete.cases(bootstrap_stat), ] %>%
  get_confidence_interval(level = 0.9)

viz_plot <- bootstrap_stat[complete.cases(bootstrap_stat), ] %>% visualize()
viz_plot <- viz_plot + shade_confidence_interval(ci)
print(viz_plot)

ci2 <- bootstrap_stat[complete.cases(bootstrap_stat), ] %>%
  get_confidence_interval(type = "se", point_estimate = obs_stat)

viz_plot <- bootstrap_stat[complete.cases(bootstrap_stat), ] %>% visualize()
viz_plot <- viz_plot + shade_confidence_interval(ci2)
print(viz_plot)

# Example where the response variable is numeric -> diff in means statistic
# Analyze dependence of hp on am

obs_stat <- mtcars %>% 
  specify(hp ~ am) %>%
  calculate(stat = "diff in means", order=c("1","0"))

permuted_stat <- mtcars %>% 
  specify(hp ~ am) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type="permute") %>%
  calculate(stat = "diff in means", order=c("1","0")) 

p_val <- permuted_stat %>%
  get_p_value(obs_stat = obs_stat, direction = "two_sided")

viz_plot <- permuted_stat %>%
  visualize()
viz_plot <- viz_plot + shade_p_value(obs_stat, direction = "two_sided")
print(viz_plot)

bootstrap_stat <- mtcars %>%
  specify(hp ~ am) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("1", "0"))

ci <- bootstrap_stat %>%
  get_confidence_interval(level = 0.9)

viz_plot <- bootstrap_stat[complete.cases(bootstrap_stat), ] %>% visualize()
viz_plot <- viz_plot + shade_confidence_interval(ci)
print(viz_plot)

ci2 <- bootstrap_stat %>%
  get_confidence_interval(type = "se", point_estimate = obs_stat)

viz_plot <- bootstrap_stat[complete.cases(bootstrap_stat), ] %>% visualize()
viz_plot <- viz_plot + shade_confidence_interval(ci2)
print(viz_plot)