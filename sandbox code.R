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
hamid_id<-asa_client$get_players(names='Hamid')[['player_id']]

hamid_xgoals <- asa_client$get_goalkeeper_xgoals(
  leagues = "mls",
  player_names = "Bill Hamid",
  split_by_game = TRUE)%>%ignore_ssl()


# pull expected goals against for all keepers
keeper_xgoals<- asa_client$get_goalkeeper_xgoals(
  leagues = "mls",
  split_by_game = TRUE)%>%ignore_ssl()
# pull expected goals against for all teams

# get all keepers
keepers<-asa_client$get_players()%>%
  filter(player_id%in%keeper_xgoals$player_id)
  
keepers_xga_added<-asa_client$get_goalkeeper_goals_added(
                                                         split_by_game=TRUE)%>%
  filter(player_id%in%keeper_xgoals$player_id)%>%
  ignore_ssl()%>%
  mutate(player_game = paste0(player_id,game_id))

test<-keepers_xga_added%>%
  filter(player_id==hamid_id)
parse_xga<-function(i){
  temp_frame <-keepers_xga_added[i,c("player_id","game_id","team_id")]
  temp_frame2<-as.data.frame(keepers_xga_added[i,][['data']])
  temp_frame2$player_id<-temp_frame$player_id
  temp_frame2$game_id<-temp_frame$game_id
  temp_frame2$team_id<-temp_frame$team_id
  return(temp_frame2#%>%
           #select(player_id,action_type,goals_added_above_avg)%>%
           #spread(key=action_type,value=goals_added_above_avg)
         )
}
keepers_xga_parsed<-do.call(bind_rows,
                            lapply(1:nrow(keepers_xga_added),parse_xga))
keepers_xga_spread<-keepers_xga_parsed%>%
  select(player_id,game_id,team_id,action_type,goals_added_above_avg)%>%
  unique()%>%
  spread(key=action_type,value=goals_added_above_avg)%>%
  inner_join(keepers,by='player_id')%>%
  arrange(desc(Shotstopping))
# each row of keepers_xga_spread should be a game, so inner join with all_combined
with_xga<-all_combined%>%
  inner_join(keepers_xga_spread,by=c("player_id","game_id","team_id"))
hamid_test<-with_xga%>%
  filter(player_name.x=='Bill Hamid')%>%
  filter(season_name == "2019")%>%
  mutate(opponent_id = case_when(team_id!=home_team_id~home_team_id,
                                                      TRUE~away_team_id),
                              home_away = case_when(team_id==home_team_id~'home',
                                                    TRUE~'away'))%>%
  inner_join(teams,by='opponent_id')%>%
  arrange(Shotstopping)%>%
  select(player_name.x,Shotstopping,xgoals_against,xgoals_gk_faced,shots_against,shots_faced,opponent_name,date_time_utc,goals_against,goal_against_ratio)

with_xga%>%
  mutate(pct_on_target = shots_faced/shots_against)%>%
  ggplot(aes(y=goal_against_ratio,x=pct_on_target,color=goals_against))+
  geom_point()+
  scale_colour_viridis_c()+
  geom_smooth(method = "lm", formula = y ~ log(x))+
  theme(legend.position='bottom')+
  ylim(0,5)

# quick lm to see if any of the XGA variables are predictive of the goals_against_ratio
summary(lm("goal_against_ratio~shots_against",
           data=with_xga%>%filter(xgoals_gk_faced!=0 & season_name%in%c("2018","2019"))))

test<-keepers_xga_parsed[c(19,817),]
team_xgoals<-asa_client$get_team_xgoals(
  leagues= "mls",
  #team_names = "D.C. United",
  split_by_game = TRUE
)%>%ignore_ssl()
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

  # find just player-season combinations where player played at least 15 games
atleast_15<- all_combined%>%
  group_by(player_name,season_name)%>%
  summarise(game_count = n_distinct(game_id))%>%
  ungroup()%>%
  filter(game_count>=15)

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

ranking2021<-all_combined%>%
  filter(season_name=="2021")%>%
  group_by(player_name,season_name)%>%
  summarise(game_count = n_distinct(game_id),
            goal_against_ratio = median(goal_against_ratio))
  
all_combined%>%
  filter(player_name=='Bill Hamid' & season_name %in% c('2019', '2020','2021'))%>%
  mutate(opponent_id = case_when(team_id!=home_team_id~home_team_id,
                                 TRUE~away_team_id),
         home_away = case_when(team_id==home_team_id~'home',
                               TRUE~'away'))%>%
  inner_join(teams,by='opponent_id')%>%
  mutate(match_date = as.Date(date_time_utc),
         olsen = case_when(match_date>as.Date('2020-10-08')~'after Olsen',
                           TRUE~ 'with Olsen'))%>%
  ggplot(aes(x=match_date,y=goal_against_ratio,color=home_away))+
  geom_point()+
  geom_text(aes(label=opponent_name))+
  labs(title = 'Bill Hamid: team xg_against/gk xg_against - 2019-2021')+
  theme(legend.position='bottom')+
  ylim(c(0,6))

teams<-asa_client$get_teams()%>%
  select('opponent_id'=team_id,'opponent_name'=team_short_name)
# find games with the highest ratio for hamid
highest_ratio<-all_combined%>%
  filter(season_name %in% c("2018","2019","2020","2021"))%>%
  filter(player_name=='Bill Hamid')%>%
  mutate(opponent_id = case_when(team_id!=home_team_id~home_team_id,
                                TRUE~away_team_id))%>%
  inner_join(teams,by='opponent_id')%>%
  arrange(desc(wgoal_against_ratio))%>%
  select(minutes_played,game_id, shots_against, goals_conceded,xgoals_gk_faced,xgoals_against,wgoal_against_ratio,opponent_name,date_time_utc)

test<-asa_client$get_game_xgoals(game_id='odMX2Nb2qY')%>%
  ignore_ssl()

combined%>%
  mutate(goal_against_ratio = xgoals_against/xgoals_gk_faced)%>%
  group_by(season_name)%>%
  summarise(goal_against_ratio = median(goal_against_ratio))

  
  



dcu_xgoals<-asa_client$get_team_xgoals(
  leagues= "mls",
  team_names = "D.C. United",
  split_by_game = TRUE
)%>%ignore_ssl()

games<-asa_client$get_games(
  leagues= "mls",
  team_names = "D.C. United"
)%>%ignore_ssl()

combined<-hamid_xgoals%>%
  inner_join(dcu_xgoals,on="game_id")%>%
  inner_join(games,on="game_id")%>%
  filter(minutes_played == expanded_minutes)%>%
  mutate(goal_against_diff = xgoals_against - xgoals_gk_faced  )
combined%>%
  mutate(goal_against_ratio = xgoals_against/xgoals_gk_faced)%>%
  group_by(season_name)%>%
  summarise(goal_against_ratio = median(goal_against_ratio))

combined%>%
  filter(season_name %in% c("2019","2020", "2021"))%>%
  mutate(home_away = case_when(team_id==home_team_id~"home",
                   TRUE~"away"))%>%
  ggplot()+
  geom_point(aes(x=xgoals_gk_faced,y=xgoals_against,color = shots_faced
                 ))+
  geom_abline(slope = 1, intercept=0)+
  #scale_colour_viridis_d(option='inferno')+
  theme(legend.position='bottom')+
  ylim(c(0,6))+
  xlim(c(0,6))

combined%>%
  mutate(game_date = as.Date(date_time_utc))%>%
  filter(season_name %in% c("2019","2020", "2021"))%>%
  ggplot()+
  geom_bar(aes(x=game_date,y=goal_against_diff),stat="identity")+
  scale_colour_viridis_d(option='inferno')+
  theme(legend.position='bottom')
  