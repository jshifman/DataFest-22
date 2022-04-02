log.files <- read_csv("/home/jonnym/Desktop/DataFest/data files/logs.csv",guess_max=2106600)
player_data <- log.files
people_data <- player_data[player_data$event_id %in% c(900:912),]
keep_col <- c("row_id"    ,"player_id"        ,"school",                    
              "wave"      ,"session"          ,"date",                           
              "event_id"  ,"event_description","event_category",                 
              "event_time","event_time_dbl","minigame_level", "total_points","total_strikes","missed_safe_invitations",
              "accepted_unsafe_invitations")

people_data <- people_data[keep_col]


people_data$date <- as.Date.character(people_data$date)

people_data <- people_data %>%
  group_by(player_id) %>%
  mutate(initial_date = min(.$date),time_since_initial = date - initial_date)
    

total_bad <- people_data %>%
  group_by(minigame_level, player_id) %>%
  summarise(total_bad = sum(missed_safe_invitations + accepted_unsafe_invitations, na.rm = T))


final_t_bad <- total_bad %>%
  group_by(minigame_level) %>%
  summarise(avg_bad = mean(total_bad))


three_strikes <- people_data %>%
  group_by(minigame_level) %>%
  summarise(total = sum(event_id==912))

ggplot(people_data,aes(x=minigame_level,))