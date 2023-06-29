## Testing and Charting WPA and XRA stability from year to year for Twenty20 competitions

library(ggpubr)
library(tidyverse)
library(ggthemes)
library(glue)

wpa_both <- readRDS("ment20/MT20_all.RDS") %>%
  filter(competition != "SA20")

df_lagged_total_per <- wpa_both %>%
  filter(balls_total >= 100) %>%
  group_by(player, competition) %>%
  arrange(season_singular) %>%
  mutate(total_xra_per_ball_lag = lag(total_XRA_per_ball),
         total_wpa_per_ball_lag = lag(total_wpa_per_ball)) %>%
  ungroup()

wpa_total_per_ball_fit <- lm(total_wpa_per_ball ~ total_wpa_per_ball_lag, data = df_lagged_total_per)
summary(wpa_total_per_ball_fit)  

xra_total_per_ball_fit <- lm(total_XRA_per_ball ~ total_xra_per_ball_lag, data = df_lagged_total_per)
summary(xra_total_per_ball_fit)  

df_lagged_total_per %>%
  ggplot(aes(x = total_wpa_per_ball_lag, y = total_wpa_per_ball)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "Win Probability Added (WPA)/Ball in Year N vs Year N+1",
       subtitle = "Balls total >= 100",
       x = "WPA/Ball in Year N",
       y = "WPA/Ball in Year N+1",
       caption = "data: cricketdata   chart: @TAlbTree")+
  scale_color_hc() +
  theme_pander()+
  theme(legend.position = "") 
ggsave(filename = glue("ment20/year_n_1/WPA_per_ball_year_n_1.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)


df_lagged_total_per %>%
  ggplot(aes(x = total_xra_per_ball_lag, y = total_XRA_per_ball)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "Total Expected Runs Added (XRA)/Ball in Year N vs Year N+1",
       subtitle = "Balls total >= 100",
       x = "Total XRA in Year N",
       y = "Total XRA in Year N+1",
       caption = "data: cricketdata   chart: @TAlbTree")+
  scale_color_hc() +
  theme_pander()+
  theme(legend.position = "") 
ggsave(filename = glue("ment20/year_n_1/XRA_per_ball_year_n_1.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)


df_lagged_total <- wpa_both %>%
  group_by(player, competition) %>%
  arrange(season_singular) %>%
  mutate(total_xra_lag = lag(total_XRA),
         total_wpa_lag = lag(total_wpa)) %>%
  ungroup()

wpa_total_fit <- lm(total_wpa ~ total_wpa_lag, data = df_lagged_total)
summary(wpa_total_fit)   

xra_total_fit <- lm(total_XRA ~ total_xra_lag, data = df_lagged_total)
summary(xra_total_fit)   

df_lagged_total %>%
  ggplot(aes(x = total_wpa_lag, y = total_wpa)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "Total Win Probability Added (WPA) in Year N vs Year N+1",
       x = "Total WPA in Year N",
       y = "Total WPA in Year N+1",
       caption = "data: cricketdata   chart: @TAlbTree")+
  scale_color_hc() +
  theme_pander()+
  theme(legend.position = "") 
ggsave(filename = glue("ment20/year_n_1/total_WPA_year_n_1.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)


df_lagged_total %>%
  ggplot(aes(x = total_xra_lag, y = total_XRA)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "Total Expected Runs Added (XRA) in Year N vs Year N+1",
       x = "Total XRA in Year N",
       y = "Total XRA in Year N+1",
       caption = "data: cricketdata   chart: @TAlbTree")+
  scale_color_hc() +
  theme_pander()+
  theme(legend.position = "") 

ggsave(filename = glue("ment20/year_n_1/total_XRA_year_n_1.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)


df_lagged_bat <- wpa_both %>%
  filter(balls_faced >= 25) %>%
  group_by(player, competition) %>%
  arrange(season_singular) %>%
  mutate(xra_bat_lag = lag(bat_XRA_per_ball),
         wpa_bat_lag = lag(bat_wpa_per_ball)) %>%
  ungroup()

xra_bat_fit <- lm(bat_XRA_per_ball ~ xra_bat_lag, data = df_lagged_bat)
summary(xra_bat_fit)   

wpa_bat_fit <- lm(bat_wpa_per_ball ~ wpa_bat_lag, data = df_lagged)
summary(wpa_bat_fit)  

df_lagged_bat %>%
  ggplot(aes(x = xra_bat_lag, y = bat_XRA_per_ball)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "Batting Expected Runs Added (XRA)/Ball in Year N vs Year N+1",
       subtitle = "Balls faced >= 25",
       x = "Batting XRA/Ball in Year N",
       y = "Batting XRA/Ball in Year N+1",
       caption = "data: cricketdata   chart: @TAlbTree")+
  scale_color_hc() +
  theme_pander()+
  theme(legend.position = "") 
ggsave(filename = glue("ment20/year_n_1/bat_XRA_year_n_1.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)

df_lagged_bat %>%
  ggplot(aes(x = wpa_bat_lag, y = bat_wpa_per_ball)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm)+
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "Batting Win Probability Added (WPA)/Ball in Year N vs Year N+1",
       subtitle = "Balls faced >= 25",
       x = "Batting WPA/Ball in Year N",
       y = "Batting WPA/Ball in Year N+1",
       caption = "data: cricketdata   chart: @TAlbTree") +
  scale_color_hc()+
  theme_pander()+
  theme(legend.position = "") 
ggsave(filename = glue("ment20/year_n_1/bat_WPA_year_n_1.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)

df_lagged_bowl <- wpa_both %>%
  filter(balls_bowled >= 25) %>%
  group_by(player, competition) %>%
  arrange(season_singular) %>%
  mutate(xra_bowl_lag = lag(bowl_XRA_per_ball),
         wpa_bowl_lag = lag(bowl_wpa_per_ball)) %>%
  ungroup()

xra_bowl_fit <- lm(bowl_XRA_per_ball ~ xra_bowl_lag, data = df_lagged_bowl)
summary(xra_bowl_fit)    

wpa_bowl_fit <- lm(bowl_wpa_per_ball ~ wpa_bowl_lag, data = df_lagged_bowl)
summary(wpa_bowl_fit)    


df_lagged_bowl %>%
  ggplot(aes(x = xra_bowl_lag, y = bowl_XRA_per_ball)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "Bowling Expected Runs Added (XRA)/Ball in Year N vs Year N+1",
       subtitle = "Balls bowled >= 25",
       x = "Bowling XRA/Ball in Year N",
       y = "Bowling XRA/Ball in Year N+1",
       caption = "data: cricketdata   chart: @TAlbTree")+
  scale_color_hc()+
  theme_pander()+
  theme(legend.position = "") 
ggsave(filename = glue("ment20/year_n_1/bowl_XRA_year_n_1.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)

df_lagged_bowl %>%
  ggplot(aes(x = wpa_bowl_lag, y = bowl_wpa_per_ball)) +
  geom_point(aes(colour = competition), alpha = 0.5) +
  geom_smooth(method = lm)+
  facet_wrap(~competition) +
  ggpubr::stat_regline_equation(label.y.npc = "top", label.x.npc = "left",
                                aes(label = ..rr.label..)) +
  labs(title = "Bowling Win Probability Added (WPA)/Ball in Year N vs Year N+1",
       subtitle = "Balls bowled >= 25",
       x = "Bowling WPA/Ball in Year N",
       y = "Bowling WPA/Ball in Year N+1",
       caption = "data: cricketdata   chart: @TAlbTree") +
  scale_color_hc()+
  theme_pander()+
  theme(legend.position = "") 
ggsave(filename = glue("ment20/year_n_1/bowl_WPA_year_n_1.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 8)
