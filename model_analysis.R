#viz plots
library(gganimate)
library(tidyverse)
library(gganimate)
library(cowplot)
library(ggridges)
library(repr)
library(shadowtext)

new_df$predictions <- as.numeric(new_df$predictions)
pff_pressures <- just_action_tracking %>% filter(event == "ball_snap")%>% 
  group_by(comb_id) %>% summarise(pressures = sum(pff_hit,pff_hurry,pff_sack, na.rm = TRUE))
model_pressures <- new_df %>% select(c(comb_id, predictions)) %>% 
  group_by(comb_id) %>% summarise(pred = sum(as.numeric(predictions)))
merged_pressures <- merge(pff_pressures, model_pressures, on = comb_id)
ggplot(merged_pressures)+geom_boxplot(aes(pressures, pred, group = pressures))+theme_classic()+xlab("PFF hit/hurry/sack")+ylab("xPa per Play")


###
#time series plot
time_cum_play <- new_df %>% select(c("team","frameId","predictions")) %>% 
  group_by(team, frameId) %>% summarize(mean_prediction = mean(predictions))
time_cum_play <- merge(time_cum_play,teams_colors_logos, by.x = "team", by.y = "team_abbr")  
  
  time_cum_play <-time_cum_play %>% arrange(frameId)
  
  timeseries_animate<-ggplot(time_cum_play,aes(x=frameId,y=mean_prediction)) +
    geom_line() +
    #geom_point(size=2) +
    geom_image(aes(image=team_logo_espn), size=.5)+
    geom_text(
      mapping = aes(x = frameId, y = mean_prediction, label = ""),
      nudge_x = 3,hjust=1,size=5,show.legend = FALSE
    )  +
    transition_reveal(frameId)+facet_wrap(~team)+theme_classic()+xlab("time (1/10 seconds)")+ylab("expected pressure")
  
  
  play_length<-length(unique(time_animate_play$frameId))
 anim_save("time_series.gif", animate(timeseries_animate, width = 720, height = 400,
          fps = 10, nframe = play_length))
  
  
#df to store rows where pressure is over 2 in first second
late_pressure_df <- data.frame()
#get scores for teams after 1 second after conceding more than 2
  #first isolate just 10 frames after snap
for(i in 1:length(levels(df_in_play$comb_id))){
    df_in_play_down <- df_in_play %>% filter(comb_id == levels(df_in_play$comb_id)[i])
    one_second_after <- df_in_play_down$snapped[1]+10
    first_second_df <- new_df %>% filter(frameId >= df_in_play_down$snapped[1] & frameId < one_second_after & comb_id == levels(df_in_play$comb_id)[i])
#then get summed scores for these
    summed_pressure <- sum(first_second_df$predictions)
    #then take just the combids where score > 2
    if(summed_pressure >2){
      late_seconds_df <- new_df %>% filter(frameId > one_second_after & comb_id == levels(df_in_play$comb_id)[i])
      late_pressure_df <- rbind(late_pressure_df,late_seconds_df)
    }

}  
just_sacks<- just_action_tracking %>% filter(event == "qb_sack"|event == "qb_strip_sack") %>% select(c(comb_id)) %>% distinct
late_pressure_df$sack <- ifelse(late_pressure_df$comb_id %in% just_sacks$comb_id,1,0)
ggplot(late_pressure_df)+geom_boxplot(aes(sack, predictions, group = sack))+theme_classic()
ggplot(merged_pressures)+geom_boxplot(aes(pressures, pred, group = pressures))+theme_classic()

#get teams and mins played by OL
new_df <- merge(just_action_tracking,merged_predictions, on= "comb_and_frame")
new_df <- new_df %>% filter(pff_role == "Pass Block")
all_OL <- new_df %>% group_by(team, nflId,playId) %>% summarise(count = n()) %>% group_by(team, nflId) %>% summarise(count = n())
by_team <- all_OL %>% group_by(team) %>% summarise(time_avg = mean(count))

library(nflfastR)
#get team branding as variable
data(teams_colors_logos)

pressure_by_team <- late_pressure_df %>% group_by(comb_id,team) %>% summarise(sacks = max(sack))%>% 
  group_by(team) %>% summarise(pressure_avg = mean(sacks))
pressure_time <- left_join(pressure_by_team,by_team, on = "team")
pressure_time <- merge(pressure_time,teams_colors_logos, by.x = "team", by.y = "team_abbr")
library(ggimage)
ggplot(pressure_time, aes(time_avg, pressure_avg))+
  geom_image(aes(image=team_logo_espn), size=.08)+
  geom_smooth(method ="lm", se = FALSE)+theme_classic()+
  xlab("Average Play Count, Pass Blockers")+ylab("Sack Percentage on High Risk Plays")
