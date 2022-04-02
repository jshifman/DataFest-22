library(tidyverse)

survey_data <- read.csv("./data files/S5_scores_cleaned.csv")

survey_wide <- reshape(survey_data,idvar="player_id",timevar="weeks",direction = "wide")

player_data <- read.csv("/home/jonnym/Desktop/DataFest/data files/player-6427031 .csv")
ncol(player_data)
player_data <- player_data[player_data$event_id %in% c(900:912),]
keep_col <- c("row_id"    ,"player_id"        ,"school",                    
              "wave"      ,"session"          ,"date",                           
              "event_id"  ,"event_description","event_category",                 
              "event_time","event_time_dbl","minigame_level", "total_points","total_strikes","missed_safe_invitations",
              "accepted_unsafe_invitations")

player_data_keep <- player_data[keep_col]


boxplot(survey_wide[,c(2:6)])

player_data$accepted_unsafe_invitations
