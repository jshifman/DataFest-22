library(tidyverse)
log.files <- read_csv("data/DataFest2022/data files/logs.csv", guess_max = 2106600, col_select = c(date, total_points, event_id, player_id, minigame_level))

people_data <- log.files
people_data$date <- as.Date.character(people_data$date)
people_data <- filter(people_data, date > "1975-01-01")

people_data <- people_data %>%
  group_by(player_id) %>%
  mutate(initial_date = min(date),time_since_initial = as.numeric(date - initial_date)/7)
  
max_level_d <- people_data %>%
  filter(event_id == 911) %>% 
  group_by(player_id) %>%
  summarise(max_level = max(minigame_level))

nrow(filter(max_level_d, max_level %in% c(8,9)))/nrow(max_level_d)

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

avg_t_bad <- read.csv("bad_choices.csv")
three_strikes <- read.csv("three_strikes.csv")

three_strikes$minigame_level <- as.factor(three_strikes$minigame_level)
avg_t_bad$minigame_level <- as.factor(avg_t_bad$minigame_level)

library(ggthemes)
ggplot() +
  theme_hc() +
  geom_col(data = three_strikes, aes(x = minigame_level, y = total), fill = "blue", alpha = .3) +
  geom_line(data = avg_t_bad, aes(x = minigame_level, y = avg_bad*8, group = 1)) +
  geom_point(data = avg_t_bad, aes(x = minigame_level, y = avg_bad*8, group = 1)) +
  scale_y_continuous("Total Minigame Losses (Bar)", sec.axis = sec_axis(~./8, name = "Average Total Bad Decisions Made (Line)")) +
  xlab("Minigame Level") +
  ggtitle("Aggregate Totals from Friends Minigame")


people_data <- filter(people_data, event_id == 911)

ggplot() +
  geom_point(data = people_data, aes(x = time_since_initial, y = total_points)) +
  geom_smooth(data = people_data, aes(x = time_since_initial, y = total_points)) +
  ylim(c(0, 50)) +
  xlab("Weeks Since the Start") +
  ylab("Total Points on Friends Minigame")
