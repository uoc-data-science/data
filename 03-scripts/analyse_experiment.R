library(ggplot2)
library(dplyr)
library(sqldf)
library(tidyr)
library(reshape2)
library(tibble)
library(infer)

experiment <- read.csv("01-raw-data/experiment/experimental_results.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
experiment <- select(experiment, "id", "ranking_based", "random_recommendations", "profit_oriented")
n_experiment <- nrow(experiment)
experiment <- filter(experiment, experiment$ranking_based != "NA" & experiment$random_recommendations != "NA" & experiment$profit_oriented != "NA")
n_del_experiment <- nrow(experiment)
ex_rb <- select(experiment, "ranking_based")
colnames(ex_rb) <- "value"
ex_rr <- select(experiment, "random_recommendations")
colnames(ex_rr) <- "value"
ex_po <- select(experiment, "profit_oriented")
colnames(ex_po) <- "value"

ci_level_1 <- 0.975
ci_level_2 <- 0.95
ci_level_3 <- 0.9

spec_rb <- specify(ex_rb, response = value)
spec_rr <- specify(ex_rr, response = value)
spec_po <- specify(ex_po, response = value)

bootstr_result_rb <- calculate(generate(spec_rb, reps = 1000), stat = "mean")
bootstr_result_rr <- calculate(generate(spec_rr, reps = 1000), stat = "mean")
bootstr_result_po <- calculate(generate(spec_po, reps = 1000), stat = "mean")

ci_rb <- get_ci(bootstr_result_rb, level = ci_level_1, type = "percentile")
ci_rr <- get_ci(bootstr_result_rr, level = ci_level_1, type = "percentile")
ci_po <- get_ci(bootstr_result_po, level = ci_level_1, type = "percentile")

lower_bound <- c(as.numeric(ci_rb[1]), as.numeric(ci_rr[1]), as.numeric(ci_po[1]))
upper_bound <- c(as.numeric(ci_rb[2]), as.numeric(ci_rr[2]), as.numeric(ci_po[2]))
bootstr_point_estimators <- c(mean(bootstr_result_rb$stat), mean(bootstr_result_rr$stat), mean(bootstr_result_po$stat))
experiment_data_table_1 <- data.frame("Recommendation" = c("Ranking based", "Random", "Profit oriented"))
experiment_data_table_1 <- cbind(experiment_data_table_1, "Lower Bound" = c(lower_bound[1],lower_bound[2],lower_bound[3]))
experiment_data_table_1 <- cbind(experiment_data_table_1, "Upper Bound" = c(upper_bound[1],upper_bound[2],upper_bound[3]))
experiment_data_table_1 <- cbind(experiment_data_table_1, "Range" = c(upper_bound[1]-lower_bound[1],upper_bound[2]-lower_bound[2],upper_bound[3]-lower_bound[3]))

ci_rb <- get_ci(bootstr_result_rb, level = ci_level_2, type = "percentile")
ci_rr <- get_ci(bootstr_result_rr, level = ci_level_2, type = "percentile")
ci_po <- get_ci(bootstr_result_po, level = ci_level_2, type = "percentile")

lower_bound <- c(as.numeric(ci_rb[1]), as.numeric(ci_rr[1]), as.numeric(ci_po[1]))
upper_bound <- c(as.numeric(ci_rb[2]), as.numeric(ci_rr[2]), as.numeric(ci_po[2]))
bootstr_point_estimators <- c(mean(bootstr_result_rb$stat), mean(bootstr_result_rr$stat), mean(bootstr_result_po$stat))
experiment_data_table_2 <- data.frame("Recommendation" = c("Ranking based", "Random", "Profit oriented"))
experiment_data_table_2 <- cbind(experiment_data_table_2, "Lower Bound" = c(lower_bound[1],lower_bound[2],lower_bound[3]))
experiment_data_table_2 <- cbind(experiment_data_table_2, "Upper Bound" = c(upper_bound[1],upper_bound[2],upper_bound[3]))
experiment_data_table_2 <- cbind(experiment_data_table_2, "Range" = c(upper_bound[1]-lower_bound[1],upper_bound[2]-lower_bound[2],upper_bound[3]-lower_bound[3]))

ci_rb <- get_ci(bootstr_result_rb, level = ci_level_3, type = "percentile")
ci_rr <- get_ci(bootstr_result_rr, level = ci_level_3, type = "percentile")
ci_po <- get_ci(bootstr_result_po, level = ci_level_3, type = "percentile")

lower_bound <- c(as.numeric(ci_rb[1]), as.numeric(ci_rr[1]), as.numeric(ci_po[1]))
upper_bound <- c(as.numeric(ci_rb[2]), as.numeric(ci_rr[2]), as.numeric(ci_po[2]))
bootstr_point_estimators <- c(mean(bootstr_result_rb$stat), mean(bootstr_result_rr$stat), mean(bootstr_result_po$stat))
experiment_data_table_3 <- data.frame("Recommendation" = c("Ranking based", "Random", "Profit oriented"))
experiment_data_table_3 <- cbind(experiment_data_table_3, "Lower Bound" = c(lower_bound[1],lower_bound[2],lower_bound[3]))
experiment_data_table_3 <- cbind(experiment_data_table_3, "Upper Bound" = c(upper_bound[1],upper_bound[2],upper_bound[3]))
experiment_data_table_3 <- cbind(experiment_data_table_3, "Range" = c(upper_bound[1]-lower_bound[1],upper_bound[2]-lower_bound[2],upper_bound[3]-lower_bound[3]))