library(tidyverse)
library(xgboost)

run_male_t20_wpa_models <- function(x){mbb_bbb_clean <- x %>%
  mutate(delivery_no = 120-balls_remaining,
         runs_gained = runs_off_bat+extras,
         run_chase = target-runs_scored_yet)  %>%
  group_by(match_id, innings) %>%
  arrange(-balls_remaining) %>%
  mutate(wickets_prior_to_ball = lag(wickets_lost_yet),
         runs_prior_to_ball = lag(runs_scored_yet),
         balls_prior_to_ball = lag(balls_remaining),
         run_chase_prior_to_ball = lag(run_chase)) %>%
  ungroup() %>%
  arrange(match_id, innings, -balls_remaining)


mbb_bbb_2022_innings1 <- mbb_bbb_clean %>%
  filter(innings == 1)

mbb_bbb_2022_innings2 <- mbb_bbb_clean %>%
  filter(innings == 2)

mt20_testing1 <- mbb_bbb_clean %>%
  filter(innings == 1) %>%
  dplyr::select(balls_remaining, runs_scored_yet, wickets_lost_yet) %>%
  as.matrix()

mt20_testing1_xrun <- mbb_bbb_clean %>%
  filter(innings == 1) %>%
  dplyr::select(wickets_prior_to_ball, runs_prior_to_ball, balls_prior_to_ball) %>%
  as.matrix()

mt20_testing2 <- mbb_bbb_clean %>%
  filter(innings == 2) %>%
  dplyr::select(balls_remaining, runs_scored_yet, wickets_lost_yet, run_chase) %>%
  as.matrix()

mt20_testing2_xrun <- mbb_bbb_clean %>%
  filter(innings == 2) %>%
  dplyr::select(wickets_prior_to_ball, runs_prior_to_ball, balls_prior_to_ball, run_chase_prior_to_ball) %>%
  as.matrix()

final_xgb_innings1_mens_xg <- xgb.load("ment20/final_xgb_innings1_mens_xg")
final_xgb_innings2_mens_xg <- xgb.load("ment20/final_xgb_innings2_mens_xg")
final_xgb_xrun_innings1_mens_xg <- xgb.load("ment20/final_xgb_xrun_innings1_mens_xg")
final_xgb_xrun_innings2_mens_xg <- xgb.load("ment20/final_xgb_xrun_innings2_mens_xg")


test_1 <- predict(final_xgb_innings1_mens_xg, mt20_testing1) %>% as.data.frame() %>% mutate_if(is.numeric, round, 4)
mbb_bbb_2022_innings1$wp <- test_1$.
test_1_xrun <- predict(final_xgb_xrun_innings1_mens_xg, mt20_testing1_xrun) %>% as.data.frame() %>% mutate_if(is.numeric, round, 4)
mbb_bbb_2022_innings1$xrun <- test_1_xrun$.

test_2 <- predict(final_xgb_innings2_mens_xg, mt20_testing2) %>% as.data.frame() %>% mutate_if(is.numeric, round, 4)
mbb_bbb_2022_innings2$wp <- test_2$.
test_2_xrun <- predict(final_xgb_xrun_innings2_mens_xg, mt20_testing2_xrun) %>% as.data.frame() %>% mutate_if(is.numeric, round, 4)
mbb_bbb_2022_innings2$xrun <- test_2_xrun$.

mbb_bbb_2022_all <- bind_rows(mbb_bbb_2022_innings1, mbb_bbb_2022_innings2)

MT20_custom_game_ids <- mbb_bbb_2022_all %>%
  filter(innings == 1) %>%
  group_by(match_id) %>%
  slice_max(balls_remaining, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(custom_match_id = paste(competition, season, start_date, batting_team, bowling_team)) %>%
  dplyr::select(match_id, custom_match_id)

logos_and_colours <- readxl::read_excel("ment20/logos_mens.xlsx")

mbb_bbb_2022_with_logos <- mbb_bbb_2022_all %>%
  left_join(logos_and_colours, by = c('batting_team' = 'team',
                                      'competition' = 'competition'), na_matches = "never") %>%
  left_join(MT20_custom_game_ids, by = c('match_id' = 'match_id'))
mbb_bbb_2022_with_logos$wicket_img <- 'https://png.pngtree.com/png-clipart/20221127/ourmid/pngtree-cricket-wickets-ball-png-image_6483453.png'

wpa_df <- mbb_bbb_2022_with_logos
}
