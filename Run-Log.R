#log.files <- read.csv("/home/jonnym/Desktop/DataFest/data files/logs.csv",nrows=100000)


people_data$date <- as.Date.character(people_data$date)


priority_data$date <- as.Date.character(priority_data$date)
people_data <- people_data %>%
  group_by(player_id) %>%
  mutate(initial_date = min(.$date),time_since_initial = date - initial_date)
  
total_bad <- people_data %>%
  group_by(minigame_level) %>%
  summarise(total_bad_dec = mean(missed_safe_invitations + accepted_unsafe_invitations,na.rm=T))

people_data$total_bad_dec <- people_data$missed_safe_invitations + people_data$accepted_unsafe_invitations

three_strikes <- people_data %>%
  group_by(minigame_level) %>%
  summarise(total = sum(event_id==912))

ggplot(people_data,aes(x=minigame_level,))