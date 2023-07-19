library(tidyverse)

wpa_summarise_team_match <- function(x){test_mbb <- x %>%
  group_by(match_id) %>%
  mutate(wp_prev = lag(wp)) %>%
  ungroup() %>%
  mutate(wp_prev = case_when(innings == 1 & balls_remaining == 119 ~ 0.5,
                             innings == 2 & balls_remaining == 119 ~ wp,
                             TRUE ~ wp_prev),
         bat_XRA = (runs_gained-xrun)) %>%
  mutate(bat_WPA = wp-wp_prev,
         bowl_WPA = -bat_WPA,
         bowl_XRA = -bat_XRA)

batters_wpa <- test_mbb %>%
  mutate(custom_match_id = paste(start_date, "vs", bowling_team)) %>%
  group_by(striker, batting_team, competition, season, match_id, custom_match_id) %>%
  summarise(total_bat_wpa = sum(bat_WPA, na.rm = TRUE),
            runs_for = sum(runs_off_bat, na.rm = TRUE),
            wickets_lost = sum(wicket, na.rm = TRUE),
            balls_faced= n(),
            total_bat_XRA = sum(bat_XRA, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_bat_wpa = round(total_bat_wpa, digits = 2))

bowlers_wpa <- test_mbb %>%
  mutate(custom_match_id = paste(start_date, "vs", batting_team)) %>%
  group_by(bowler, bowling_team, competition, season, match_id, custom_match_id) %>%
  summarise(total_bowl_wpa = sum(bowl_WPA, na.rm = TRUE),
            wickets_taken = sum(wicket, na.rm = TRUE),
            runs_against = sum(runs_gained, na.rm = TRUE),
            balls_bowled= n(),
            total_bowl_XRA = sum(bowl_XRA, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_bowl_wpa = round(total_bowl_wpa, digits = 2))

wpa_bat_only <- batters_wpa %>%
  rename(player = striker,
         team = batting_team)

wpa_bowl_only <- bowlers_wpa %>%
  rename(player = bowler,
         team = bowling_team)

logos_and_colours <- readxl::read_excel("ment20/logos_mens.xlsx")


wpa_both <- wpa_bat_only %>%
  full_join(wpa_bowl_only, by = c('player' = 'player', 'team' = 'team', 
                                  'competition' = 'competition', 'season' = 'season',
                                  'match_id' = 'match_id',
                                  'custom_match_id' = 'custom_match_id'), na_matches = "never")%>%
  mutate(total_bat_wpa = replace_na(total_bat_wpa, 0),
         total_bowl_wpa = replace_na(total_bowl_wpa, 0),
         total_bat_XRA = replace_na(total_bat_XRA, 0),
         total_bowl_XRA = replace_na(total_bowl_XRA, 0),
         balls_bowled = replace_na(balls_bowled, 0),
         balls_faced = replace_na(balls_faced, 0)) %>%
  mutate_if(is.integer, as.numeric) %>%
  group_by(player, season, match_id, custom_match_id) %>%
  mutate(team = unique(team[!is.na(team)])) %>%
  replace(., is.na(.),0)%>%
  ungroup()%>%
  group_by(player, team, competition, season, match_id, custom_match_id) %>%
  summarise(across(c(1:10), sum)) %>%
  ungroup() %>%
  mutate(bowl_wpa_per_ball = (total_bowl_wpa/balls_bowled)*100,
         bowl_wpa_per_ball = replace_na(bowl_wpa_per_ball, 0),
         bowl_XRA_per_ball = (total_bowl_XRA/balls_bowled),
         bowl_XRA_per_ball = replace_na(bowl_XRA_per_ball, 0),
         bowl_economy = runs_against/(balls_bowled/6),
         bowl_average = runs_against/wickets_taken,
         total_bowl_wpa = total_bowl_wpa*100,
         bat_wpa_per_ball = (total_bat_wpa/balls_faced)*100,
         bat_wpa_per_ball = replace_na(bat_wpa_per_ball, 0),
         bat_XRA_per_ball = (total_bat_XRA/balls_faced),
         bat_XRA_per_ball = replace_na(bat_XRA_per_ball, 0),
         strike_rate = runs_for/(balls_faced/6),
         average = runs_for/wickets_lost,
         total_bat_wpa = total_bat_wpa*100,
         total_wpa = total_bowl_wpa+total_bat_wpa,
         total_XRA = total_bowl_XRA+total_bat_XRA,
         balls_total = balls_bowled+balls_faced,
         total_wpa_per_ball = total_wpa/balls_total,
         total_XRA_per_ball = total_XRA/balls_total,
         season_singular = case_when(season == "2007/08" ~ "2007",
                                     season == "2008/09" ~ "2008",
                                     season == "2009/10" ~ "2009",
                                     season == "2010/11" ~ "2010",
                                     season == "2011/12" ~ "2011",
                                     season == "2012/13" ~ "2012",
                                     season == "2013/14" ~ "2013",
                                     season == "2014/15" ~ "2014",
                                     season == "2015/16" ~ "2015",
                                     season == "2016/17" ~ "2016",
                                     season == "2017/18" ~ "2017",
                                     season == "2018/19" ~ "2018",
                                     season == "2019/20" ~ "2019",
                                     season == "2020/21" ~ "2020",
                                     season == "2021/22" ~ "2021",
                                     season == "2022/23" ~ "2022",
                                     TRUE ~ season),
         team = case_when(team == "Northern Districts" ~ "Northern Brave",
                          team == "Otago" ~ "Otago Volts",
                          team == "Central Districts" ~ "Central Stags",
                          team == "Wellington" ~ "Wellington Firebirds",
                          team == "Canterbury" ~ "Canterbury Kings",
                          team == "Auckland" ~ "Auckland Aces",
                          team == "Rising Pune Supergiant" ~ "Rising Pune Supergiants",
                          team == "Kings XI Punjab" ~ "Punjab Kings",
                          TRUE ~ team)) %>%
  mutate_if(is.numeric, round, 2)%>%
  left_join(logos_and_colours, by = c('team' = 'team',
                                      'competition' = 'competition'), na_matches = "never") %>%
  group_by(team, competition, season, match_id, custom_match_id) %>%
  summarise(total_bowl_wpa = sum(total_bowl_wpa),
            total_bat_wpa = sum(total_bat_wpa)) %>%
  ungroup()
}
