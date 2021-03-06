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

max_weeks <- player_summary$max_weeks
col_num <- function(x){
  if (x == 0)
    return(2)
  if (x == 3)
    return(3)
  if (x == 6)
    return(4)
  if (x == 12)
    return(5)
  if (x == 24)
    return(6)
}
max_week_value <- c()
for (i in 1:nrow(player_summary)) {
  max_week_value[i] = player_summary[i, col_num(max_weeks[i])]
}
player_summary$max_week_value <- max_week_value
player_summary <- mutate(player_summary, perc_diff = (max_week_value-S5_mean.0)/S5_mean.0)
player_summary <- filter(player_summary, player_id != 6486018)

boxplot(perc_diff~max_weeks, player_summary)

write.csv(player_summary, file = "player_survey_summary.csv")



