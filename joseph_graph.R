survey_data <- read.csv("data/DataFest2022/data files/S5_scores_cleaned.csv")

library(tidyverse)
sum_data <- group_by(survey_data, weeks) %>%
  summarise(mean_score = mean(S5_mean), sd_score = sd(S5_mean))

ggplot(sum_data) +
  geom_line(aes(x = weeks, y = mean_score), color = "blue") +
  geom_line(aes(x = weeks, y = mean_score-sd_score), color = "cyan") +
  geom_line(aes(x = weeks, y = mean_score+sd_score), color = "cyan")


boxplot(S5_mean~weeks, survey_data)


player_summary <- reshape(survey_data, idvar= "player_id", timevar = "weeks", direction = "wide")
player_summary <- drop_na(player_summary, player_id, S5_mean.0)
surv <- group_by(survey_data, player_id) %>%
  summarise(max_weeks = max(weeks))

player_summary <- left_join(player_summary, surv)

player_summary$max_weeks <- factor(player_summary$max_weeks,
                                   levels = c("S5_mean.0", "S5_mean3",
                                              "S5_mean.6", "S5_mean.12", "S5_mean.24"))
player_summary$player_id <- factor(player_summary$player_id)
max_weeks <- player_summary$max_weeks
max_week_value <- c()
for (i in 1:nrow(player_summary)) {
  max_week_value[i] = player_summary[i, as.character(max_weeks[i])]
}
player_summary$max_week_value <- as.numeric(player_summary$max_week_value)
player_summary <- mutate(player_summary, perc_diff = (max_week_value-S5_mean.0))

player_summary$max_weeks <- factor(player_summary$max_weeks, labels = c(0,3,6,12,24))

boxplot(perc_diff~max_weeks, player_summary)
