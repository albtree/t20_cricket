## Testing whether Bowling or Batting contributes more to WPA

MT20_bbb <- readRDS("ment20/MT20_bbb.RDS")
wpa_both <- readRDS("ment20/MT20_all.RDS") 


MT20_wins <- MT20_bbb %>%
  mutate(win = case_when(innings == 1 & innings1_total>innings2_total ~ 1,
                         innings == 1 & innings1_total<innings2_total ~ 0,
                         innings == 2 & innings2_total>innings1_total ~ 1,
                         innings == 2 & innings2_total<innings1_total ~ 0,
                         innings1_total==innings2_total ~ 0)) %>%
  drop_na(innings1_total, innings2_total) %>%
  dplyr::select(match_id, competition, season, batting_team, bowling_team, win) %>%
  group_by(match_id, competition, season, batting_team) %>%
  summarise(win = max(win)) %>%
  ungroup() %>%
  group_by(competition, season, batting_team) %>%
  summarise(wins = sum(win)) %>%
  ungroup()%>%
  rename(team = batting_team)

MT20_losses <- MT20_bbb %>%
  mutate(win = case_when(innings == 1 & innings1_total>innings2_total ~ 1,
                         innings == 1 & innings1_total<innings2_total ~ 0,
                         innings == 2 & innings2_total>innings1_total ~ 1,
                         innings == 2 & innings2_total<innings1_total ~ 0,
                         innings1_total==innings2_total ~ 0)) %>%
  drop_na(innings1_total, innings2_total) %>%
  dplyr::select(match_id, competition, season, batting_team, bowling_team, win) %>%
  group_by(match_id, competition, season, batting_team) %>%
  summarise(win = max(win)) %>%
  ungroup() %>%
  filter(win == 0) %>%
  group_by(competition, season, batting_team) %>%
  summarise(losses = n()) %>%
  ungroup() %>%
  rename(team = batting_team)

MT20_records <- MT20_wins %>%
  left_join(MT20_losses, by = c('competition' = 'competition',
                                'season' = 'season', 
                                'team' = 'team'), na_matches = "never") %>%
  mutate(games_played = wins+losses,
         win_percentage = wins/games_played,
         team = case_when(team == "Northern Districts" ~ "Northern Brave",
                          team == "Otago" ~ "Otago Volts",
                          team == "Central Districts" ~ "Central Stags",
                          team == "Wellington" ~ "Wellington Firebirds",
                          team == "Canterbury" ~ "Canterbury Kings",
                          team == "Auckland" ~ "Auckland Aces",
                          team == "Rising Pune Supergiant" ~ "Rising Pune Supergiants",
                          team == "Kings XI Punjab" ~ "Punjab Kings",
                          TRUE ~ team))

wpa_xra_records <- wpa_both %>%
  group_by(team, competition, season, logo, team_colours) %>%
  summarise(across(c(total_bat_wpa, total_bowl_wpa, total_wpa, total_bat_XRA, total_bowl_XRA, total_XRA, total_wpa_per_ball, total_XRA_per_ball,
                     bat_XRA_per_ball, bowl_XRA_per_ball), sum)) %>%
  ungroup() %>%
  left_join(MT20_records, by = c('competition' = 'competition',
                                   'season' = 'season', 
                                   'team' = 'team'), na_matches = "never") %>%
  mutate(bat_wpa_title = "Batting WPA",
         bowl_wpa_title = "Bowling WPA")

fit <- lm(win_percentage ~ total_bat_wpa, data = wpa_xra_records)
summary(fit)

fit <- lm(win_percentage ~ total_bowl_wpa, data = wpa_xra_records)
summary(fit)

fit <- lm(win_percentage ~ total_bowl_wpa + total_bat_wpa, data = wpa_xra_records)
summary(fit)

wpa_xra_records %>%
  ggplot(aes()) +
  geom_point(aes(x = total_bat_wpa, y = win_percentage, colour = bat_wpa_title), alpha = 0.5) +
  geom_smooth(aes(x = total_bat_wpa, y = win_percentage, colour = bat_wpa_title),alpha = 0.6, method = lm) +
  geom_point(aes(x = total_bowl_wpa, y = win_percentage, colour  = bowl_wpa_title), alpha = 0.5) +
  geom_smooth(aes(x = total_bowl_wpa, y = win_percentage, colour  = bowl_wpa_title),alpha = 0.6, method = lm) +
  labs(title = "Bowling WPA appears to be very slightly more important than Batting WPA in Twenty20 cricket",
       subtitle = "T20 Competitions: IPL, Big Bash, Vitality Blast, SA20, Super Smash League,",
       x = "Win Probability Added (WPA)",
       y = "Team Win Percentage",
       caption = "data: cricketdata   chart: @TAlbTree",
       colour = "WPA Type")+
  theme_pander() +
  scale_color_solarized() +
  theme(plot.title = element_text(size = 12))

ggsave(filename = glue("ment20/bat_vs_bowl_testing/WPA_bat_bowl_record.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 5)
               

fit <- lm(win_percentage ~ total_bat_XRA, data = wpa_xra_records)
summary(fit)

fit <- lm(win_percentage ~ total_bowl_XRA, data = wpa_xra_records)
summary(fit)


wpa_xra_records %>%
  ggplot(aes(x = total_bowl_wpa, y = win_percentage)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "How does Bowling Win Probability Added (WPA) correlate with\nTeam Win Percentage in Twenty20 cricket?",
       subtitle = "",
       x = "Total Bowling WPA",
       y = "Win Percentage",
       caption = "data: cricketdata   chart: @TAlbTree")+
  scale_color_hc() +
  theme_pander()+
  theme(legend.position = "") 

ggsave(filename = glue("ment20/bat_vs_bowl_testing/WPA_win_perc_bowl_by_team.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)

wpa_xra_records %>%
  ggplot(aes(x = total_bat_wpa, y = win_percentage)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "How does Batting Win Probability Added (WPA) correlate with\nTeam Win Percentage in Twenty20 cricket?",
       subtitle = "",
       x = "Total Batting WPA",
       y = "Win Percentage",
       caption = "data: cricketdata   chart: @TAlbTree")+
  scale_color_hc() +
  theme_pander()+
  theme(legend.position = "") 

ggsave(filename = glue("ment20/bat_vs_bowl_testing/WPA_win_perc_bat_by_team.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)


fit <- lm(win_percentage ~ total_bat_XRA, data = wpa_xra_records)
summary(fit)


wpa_xra_records %>%
  ggplot(aes(x = total_bat_XRA, y = win_percentage)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "bottom", label.x.npc = "middle",
                                aes(label = ..rr.label..)) +
  labs(title = "How does Batting Expected Runs Added (XRA) correlate with\nWin Percentage in Twenty20 cricket?",
       subtitle = "",
       x = "Total Batting XRA",
       y = "Win Percentage",
       caption = "data: cricketdata   chart: @TAlbTree")+
  scale_color_hc() +
  theme_pander()+
  theme(legend.position = "") 

ggsave(filename = glue("ment20/bat_vs_bowl_testing/XRA_win_perc_bat_by_team.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)

fit <- lm(win_percentage ~ total_bowl_XRA, data = wpa_xra_records)
summary(fit)

wpa_xra_records %>%
  ggplot(aes(x = total_bowl_XRA, y = win_percentage)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "How does Bowling Expected Runs Added (XRA) Added correlate with\nTeam Win Percentage in Twenty20 cricket?",
       subtitle = "",
       x = "Total Bowling XRA",
       y = "Team Win Percentage",
       caption = "data: cricketdata   chart: @TAlbTree")+
  scale_color_hc() +
  theme_pander() +
  theme(legend.position = "") 


ggsave(filename = glue("ment20/bat_vs_bowl_testing/XRA_win_perc_bowl_by_team.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)

