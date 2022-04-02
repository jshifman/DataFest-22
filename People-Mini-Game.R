library(tidyverse)
library(ggplot2)

survey_data <- read.csv("../data files/S5_scores_cleaned.csv")
survey_wide <- reshape(survey_data,idvar="player_id",timevar="weeks",direction = "wide")

player_data <- read.csv("/home/jonnym/Desktop/DataFest/data files/player-6486029.csv")
#player_data <- log.files
people_data <- player_data[player_data$event_id %in% c(900:912),]
keep_col <- c("row_id"    ,"player_id"        ,"school",                    
              "wave"      ,"session"          ,"date",                           
              "event_id"  ,"event_description","event_category",                 
              "event_time","event_time_dbl","minigame_level", "total_points","total_strikes","missed_safe_invitations",
              "accepted_unsafe_invitations")

people_data <- people_data[keep_col]

general_data <- player_data[player_data$event_id == 1005,]
keep_col <- c("row_id"    ,"player_id"        ,"school",                    
              "wave"      ,"session"          ,"date",                           
              "event_id"  ,"event_description","event_category",                 
              "event_time","event_time_dbl","minigame_level", "skill_id",
              "old_skill_point","new_skill_point")
general_data <- general_data[keep_col]


refusal_data <- player_data[player_data$event_id==515,]
keep_col <- c("row_id"    ,"player_id"        ,"school",                    
              "wave"      ,"session"          ,"date",                           
              "event_id"  ,"event_description","event_category",                 
              "event_time","event_time_dbl","minigame_level", "skill_id",
              "player_points","opponent_points")
refusal_data <- refusal_data[keep_col]


priority_data <- player_data[player_data$event_id==806,]
keep_col <- c("row_id"    ,"player_id"        ,"school",                    
              "wave"      ,"session"          ,"date",                           
              "event_id"  ,"event_description","event_category",                 
              "event_time","event_time_dbl","minigame_level", "skill_id","priority_type_2")
priority_data <- priority_data[keep_col]
priority_data$priority_type_2 <- factor(priority_data$priority_type_2,labels = c("Health","Money","School","Friends","Happiness","Family"))



player_data_keep <- bind_rows(people_data,priority_data)
player_data_keep <- bind_rows(player_data_keep,refusal_data)#,by="player_id")
player_data_keep <- bind_rows(player_data_keep,general_data)#,by="player_id")
player_data_keep <- bind_rows(player_data_keep,survey_wide)#,by="player_id")

write.csv(people_data,"People-Mini-Game-Data-Player-6427031.csv")

ext_data <- read.csv("/home/jonnym/Desktop/DataFest/DataFest-22/Mini-Game-Extra.csv")
player_data_keep <- bind_rows(player_data_keep,ext_data)


ggplot(subset(player_data_keep, !is.na(priority_type_2)),aes(x=priority_type_2)) +
  geom_bar(stat="count")
