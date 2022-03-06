rm(list=ls())
library(tidyr)
library(itscalledsoccer)
library(dplyr)
library(ggplot2)

# create a function to ignore SSL (this is safer than doing it globally)
ignore_ssl<-function(expr){
  return(httr::with_config(httr::config(ssl_verifypeer=0L,
                                        verbose=TRUE),
                           expr))
}
asa_client <- httr::with_config(httr::config(ssl_verifypeer=0L,
                                             verbose=TRUE),
                                AmericanSoccerAnalysis$new())
# pull expected goals against for all keepers
keeper_xgoals<- asa_client$get_goalkeeper_xgoals(
  leagues = "mls",
  split_by_game = TRUE)%>%ignore_ssl()

# get all keepers
keepers<-asa_client$get_players()%>%
  filter(player_id%in%keeper_xgoals$player_id)

# pull team xg
team_xgoals<-asa_client$get_team_xgoals(
  leagues= "mls",
  #team_names = "D.C. United",
  split_by_game = TRUE
)%>%ignore_ssl()
# pull game level data to retrieve opponents
all_games<-asa_client$get_games(
  leagues= "mls",
  #team_names = "D.C. United"
)%>%ignore_ssl()

# combine tables for all keepers
all_combined<-keepers%>%
  inner_join(keeper_xgoals,by=c('player_id'))%>%
  inner_join(team_xgoals,by=c('competition', 'team_id', 'game_id'))%>%
  inner_join(all_games,by='game_id')%>%
  filter(minutes_played == expanded_minutes)%>%
  mutate(goal_against_diff = xgoals_against - xgoals_gk_faced,
         goal_against_ratio = xgoals_against/xgoals_gk_faced,
         # weight goal_against_ratio by goals_against so highest distortions are at the top,
         wgoal_against_ratio = xgoals_against^2/xgoals_gk_faced)

all_combined%>%
  group_by(player_name,season_name)%>%
  summarise(game_count = n_distinct(game_id),
            goal_against_ratio = median(goal_against_ratio))%>%
  ungroup()%>%
  filter(game_count>13)%>%
  mutate(hamid = case_when(player_name=='Bill Hamid' ~ 'Bill Hamid',
                           TRUE ~ "Other keeper"),
         season_num = as.numeric(season_name))%>%
  filter(season_num>=2018)%>%
  ggplot(aes(x=season_name,y=goal_against_ratio,color=hamid))+
  geom_jitter(height=0,width=.2)+
  #geom_text(aes(label=player_name),position='dodge')+
  theme_minimal()+
  theme(legend.position='bottom')+
  labs(x = 'season',
       y = 'median (team xg_against / keeper xg_against)',
       title = 'MLS goalkeepers, median team_xg_against/keeper_xg_against\n(higher ratio means keeper faces worse shot placement)',
       color = "")+
  ylim(c(.75,1.8))

ranked_2021<-all_combined%>%
  filter(season_name=='2021')%>%
  group_by(player_name)%>%
  summarise(goal_against_ratio = median(goal_against_ratio,na.rm=TRUE),
            game_count = n_distinct(game_id))%>%
  ungroup()%>%
  filter(game_count>13)%>%
  arrange(desc(goal_against_ratio))