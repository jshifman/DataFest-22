library(tidyverse)
log.files <- read_csv("data/DataFest2022/data files/logs.csv",
                      guess_max = 2106600,
                      col_select = c(date, total_points, event_id, player_id, minigame_level, missed_safe_invitations, accepted_unsafe_invitations))

people_data <- log.files
people_data$date <- as.Date.character(people_data$date)
people_data <- filter(people_data, date > "1975-01-01")

people_data <- people_data %>%
  group_by(player_id) %>%
  mutate(initial_date = min(date),time_since_initial = as.numeric(date - initial_date)/7)

player_demos <- read.csv("Cleaned_datset.csv")

people_data <- left_join(people_data, player_demos, by = "player_id")
  
max_level_d <- people_data %>%
  filter(event_id == 911) %>% 
  group_by(player_id) %>%
  summarise(max_level = max(minigame_level))

nrow(filter(max_level_d, max_level %in% c(8,9)))/nrow(max_level_d)

total_bad <- people_data %>%
  group_by(minigame_level, player_id) %>%
  summarise(total_bad_dec = sum(missed_safe_invitations + accepted_unsafe_invitations, na.rm = TRUE), avatar_age)

avg_t_bad <- total_bad %>%
  group_by(minigame_level, avatar_age) %>%
  summarise(avg_bad = mean(total_bad_dec,na.rm=T))
avg_t_bad <- drop_na(avg_t_bad)

three_strikes <- people_data %>%
  group_by(minigame_level) %>%
  summarise(total = sum(event_id.x==912))
three_strikes <- drop_na(three_strikes)

write.csv(avg_t_bad, file = "bad_choices.csv")
write.csv(three_strikes, file = "three_strikes.csv")

avg_t_bad <- read.csv("bad_choices.csv")
three_strikes <- read.csv("three_strikes.csv")

three_strikes$minigame_level <- as.factor(three_strikes$minigame_level)
avg_t_bad$minigame_level <- as.factor(avg_t_bad$minigame_level)
avg_t_bad$avatar_age <- as.factor(avg_t_bad$avatar_age)


library(ggthemes)
ggplot() +
  theme_hc() +
  geom_col(data = three_strikes, aes(x = minigame_level, y = total), fill = "blue", alpha = .3) +
  geom_line(data = avg_t_bad, aes(x = minigame_level, y = avg_bad*3, group = avatar_age, color = avatar_age)) +
  geom_point(data = avg_t_bad, aes(x = minigame_level, y = avg_bad*3, group = avatar_age, color = avatar_age)) +
  scale_y_continuous("Total Minigame Losses (Bar)", sec.axis = sec_axis(~./3, name = "Average Total Bad Decisions Made (Line)")) +
  xlab("Minigame Level") +
  ggtitle("Aggregate Totals from Friends Minigame") +
  labs(color = "Avatar Age")


people_data <- filter(people_data, event_id.x == 911)
survey_data <- read.csv("data/DataFest2022/data files/S5_scores_cleaned.csv")
survey_data <- left_join(survey_data, player_demos, by = "player_id")

write.csv(survey_data, file = "survey_with_demo.csv")
write.csv(people_data, file = "people.csv")

library(gridExtra)
p1 <- ggplot() +
  theme_hc() +
  geom_point(data = filter(people_data, avatar_gender == "Male"), aes(x = time_since_initial, y = total_points)) +
  geom_smooth(data = filter(people_data, avatar_gender == "Male"), aes(x = time_since_initial, y = total_points), se = F) +
  ylim(c(-60,60)) +
  xlab("Weeks Since Starting the Game") +
  ylab("Total Points on Friends Minigame") +
  ggtitle("Male Players")

p2 <- ggplot() +
  theme_hc() +
  geom_smooth(data = filter(survey_data, avatar_gender == "Male"), aes(x = weeks, y = S5_mean), se = F) +
  geom_point(data = filter(survey_data, avatar_gender == "Male"), aes(x = weeks, y = S5_mean), color = "blue") +
  ylim(c(3,4)) +
  ylab("Survey Score") +
  xlab("Weeks Since Starting the Game") +
  ggtitle("Male Players Survey Scores")

p3 <- ggplot() +
  theme_hc() +
  geom_point(data = filter(people_data, avatar_gender == "Female"), aes(x = time_since_initial, y = total_points)) +
  geom_smooth(data = filter(people_data, avatar_gender == "Female"), aes(x = time_since_initial, y = total_points), se = F, color = "red") +
  ylim(c(-60,60)) +
  xlab("Weeks Since Starting the Game") +
  ylab("Total Points on Friends Minigame") +
  ggtitle("Female Players")

p4 <- ggplot() +
  theme_hc() +
  geom_smooth(data = filter(survey_data, avatar_gender == "Female"), aes(x = weeks, y = S5_mean), color = "red", se = F) +
  geom_point(data = filter(survey_data, avatar_gender == "Female"), aes(x = weeks, y = S5_mean), color = "red") +
  ylim(c(3,4)) +
  ylab("Survey Score") +
  xlab("Weeks Since Starting the Game") +
  ggtitle("Female Players Survey Scores")

grid.arrange(p1, p2, p3, p4)


