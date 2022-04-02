library(tidyverse)
log.files <- read_csv("data/DataFest2022/data files/logs.csv", guess_max = 2106600)

people_data <- log.files
people_data$date <- as.Date.character(people_data$date)


people_data <- people_data %>%
  group_by(player_id) %>%
  mutate(initial_date = min(.$date),time_since_initial = date - initial_date)
  
total_bad <- people_data %>%
  group_by(minigame_level, player_id) %>%
  summarise(total_bad_dec = sum(missed_safe_invitations + accepted_unsafe_invitations, na.rm = TRUE))

avg_t_bad <- total_bad %>%
  group_by(minigame_level) %>%
  summarise(avg_bad = mean(total_bad_dec,na.rm=T))
avg_t_bad <- drop_na(avg_t_bad)

three_strikes <- people_data %>%
  group_by(minigame_level) %>%
  summarise(total = sum(event_id==912))
three_strikes <- drop_na(three_strikes)

write.csv(avg_t_bad, file = "bad_choices.csv")
write.csv(three_strikes, file = "three_strikes.csv")

ggplot(people_data,aes(x=minigame_level,))