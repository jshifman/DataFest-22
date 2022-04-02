log.files <- read_csv("/home/jonnym/Desktop/DataFest/data files/logs.csv", guess_max = 2106600)


people_data$date <- as.Date.character(people_data$date)


priority_data$date <- as.Date.character(priority_data$date)
people_data <- people_data %>%
  group_by(player_id) %>%
  mutate(initial_date = min(.$date),time_since_initial = date - initial_date)
  
total_bad <- people_data %>%
  group_by(minigame_level, player_id) %>%
  summarise(total_bad_dec = missed_safe_invitations + accepted_unsafe_invitations)

avg_t_bad <- total_bad %>%
  group_by(minigame_level) %>%
  summarise(avg_bad = mean(total_bad_dec,na.rm=T))

three_strikes <- people_data %>%
  group_by(minigame_level) %>%
  summarise(total = sum(event_id==912))

ggplot(people_data,aes(x=minigame_level,))