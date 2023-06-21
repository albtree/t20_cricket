## Indian Premier League Salaries vs WPA and XRA
library(tidyverse)
library(fuzzyjoin)
library(ggimage)
library(nflverse)
library(ggbeeswarm)
library(gtable)
library(gtExtras)


# No longer needed --------------------------------------------------------


ipl_salaries_csv_2023 <- read.csv("ment20/ipl_salaries_23.csv")%>%
  mutate(Base.Salary = case_when(Players == "Obed McCoy" ~ "$90k",
                                 TRUE ~ Base.Salary),
         salary_indicator = str_extract(Base.Salary, "[mk]"),
         Base.Salary = str_replace(Base.Salary, "[$]", ""),
         Base.Salary = str_replace(Base.Salary, "[mk]", ""),
         Base.Salary = as.numeric(Base.Salary),
         salary_m = case_when(salary_indicator == "k" ~ Base.Salary/1000,
                            TRUE ~ Base.Salary)) %>%
  separate_wider_delim(Players,delim = " ", names = c("first_name", "last_name"), 
                       too_few = "align_start", too_many = "merge") %>%
  mutate(first_name = substr(first_name, 1, 1),
         player_fuzzy = paste(first_name, "", last_name),
         Team = case_when(Team == "Royal Challengers" ~ "Royal Challengers Bangalore",
                          TRUE ~ Team)) %>%
  rename(team_fuzzy = Team) %>%
  select(player_fuzzy, team_fuzzy, salary_m, Status, Contract)

wpa_ipl_2023 <- readRDS("ment20/MT20_all.RDS") %>%
  filter(competition == "Indian Premier League", season_singular == 2023) %>%
  separate_wider_delim(player,delim = " ", names = c("first_name", "last_name"), 
                       too_few = "align_start", too_many = "merge") %>%
  mutate(first_name = substr(first_name, 1, 1),
         player = paste(first_name, "", last_name)) %>%
  select(player, everything(), -first_name, -last_name) %>%
  stringdist_left_join(ipl_salaries_csv_2023, by = c('player' = 'player_fuzzy', 'team'='team_fuzzy'), max_dist = 1, 
                       distance_col = "distance") %>%
  group_by(player) %>%
  slice_min(player.distance) %>%
  ungroup() %>%
  mutate(wpa_per_100k = total_wpa/(salary_m*10)) %>%
  mutate(merge_team = clean_player_names(team, lowercase = TRUE))


col <- as.character(wpa_ipl_2023$team_colours)
names(col) <- as.character(wpa_ipl_2023$team)
col2 <- as.character(wpa_ipl_2023$team_colour2)
names(col2) <- as.character(wpa_ipl_2023$team)

transparent <- function(img) {
  magick::image_fx(img, expression = "1*a", channel = "alpha")
}


wpa_ipl_2023 %>%
  filter(Status == "Local", balls_total > 100) %>%
  ggplot() +
  geom_image(aes(image = logo, y = total_wpa, x = salary_m), 
             image_fun = transparent, asp = 2, size = 0.04, nudge_y = 30) +
  geom_label(aes(x = salary_m, y = total_wpa, label = player, colour = team), size = 3) +
  scale_x_reverse()+
  scale_colour_manual(values = col) +
  theme_gdocs() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10)) +
  ylim(min(wpa_ipl_2023$total_wpa), max(wpa_ipl_2023$total_wpa+30)) +
  labs(x = "Salary (USD$ Mill)",
       y = "Total WPA",
       title = "Cost Efficiency for Overseas IPL Players for the 2023 Season",
       subtitle = "Minmum 100 balls",
       caption = "Chart & WPA Model: @TAlbTree. Data: cricketdata")




# All Salaries ------------------------------------------------------------


ipl_auction_2023 <- read.csv('ment20/ipl_2023_auction_result.csv')%>%
  mutate(details = str_replace_all(details,"-", " ")) %>%
  separate_wider_delim(details, delim = " ", names = c("first_name", "last_name", "position", "price_lakhs", "price_dollars_thousands"),
                       too_few = "align_end", too_many = "merge") %>%
    mutate(player_full = paste(first_name, last_name)) %>%
  separate_wider_delim(price_dollars_thousands, delim = " ", names = c('price_lakhs2', 'price_dollars_thousands'),
                       too_few = "align_start", too_many = "merge") %>%
  mutate(last_name = case_when(price_lakhs == "BAT" ~ paste(last_name, position),
                               TRUE ~ last_name),
         position = case_when(price_lakhs == "BAT" ~ price_lakhs,
                              TRUE ~ position),
         price_lakhs = case_when(price_lakhs == "BAT" ~ price_lakhs2,
                              TRUE ~ price_lakhs),
         last_name = case_when(price_lakhs == "AR" ~ paste(last_name, position),
                               TRUE ~ last_name),
         position = case_when(price_lakhs == "AR" ~ price_lakhs,
                              TRUE ~ position),
         price_lakhs = case_when(price_lakhs == "AR" ~ price_lakhs2,
                                 TRUE ~ price_lakhs),
         last_name = case_when(price_lakhs == "BOW" ~ paste(last_name, position),
                               TRUE ~ last_name),
         position = case_when(price_lakhs == "BOW" ~ price_lakhs,
                              TRUE ~ position),
         price_lakhs = case_when(price_lakhs == "BOW" ~ price_lakhs2,
                                 TRUE ~ price_lakhs),
         last_name = case_when(price_lakhs == "BAT/WK" ~ paste(last_name, position),
                               TRUE ~ last_name),
         position = case_when(price_lakhs == "BAT/WK" ~ price_lakhs,
                              TRUE ~ position),
         price_lakhs = case_when(price_lakhs == "BAT/WK" ~ price_lakhs2,
                                 TRUE ~ price_lakhs),
         price_lakhs = as.numeric(price_lakhs),
         price_lakhs2 = as.numeric(price_lakhs2),
         price_dollars_thousands = as.numeric(price_dollars_thousands),
         price_dollars_thousands = replace_na(price_dollars_thousands, 99999),
         price_dollars_thousands = if_else(price_dollars_thousands == 99999, price_lakhs2, price_dollars_thousands),
         position_detail = case_when(position == "BOW" ~ "Bowler",
                                     position == "BAT" ~ "Batter",
                                     position == "BAT/WK" ~ "Batter/Wicket Keeper",
                                     position == "AR" ~ "All Rounder")) %>%
  mutate(merge_team = clean_player_names(team, lowercase = TRUE)) %>%
  mutate(first_name = substr(first_name, 1, 1),
         player_fuzzy = paste(first_name, "", last_name),
         player_name_first = word(player_fuzzy, 1),
         player_name_last = word(player_fuzzy, -1),
         player_fuzzy = paste(player_name_first, "", player_name_last)) %>%
  rename(team_fuzzy = team) %>%
  select(player_fuzzy, player_full, merge_team, position, position_detail, price_lakhs, price_dollars_thousands)

wpa_ipl_2023 <- readRDS("ment20/MT20_all.RDS") %>%
  filter(competition == "Indian Premier League", season_singular == 2023)%>%
  mutate(merge_team = clean_player_names(team, lowercase = TRUE),
         player = str_replace_all(player,"[(1234567890)]", ""),
         player = str_replace_all(player, "-", " ")) %>%
  separate_wider_delim(player,delim = " ", names = c("first_name", "last_name"), 
                       too_few = "align_start", too_many = "merge", cols_remove = FALSE) %>%
  mutate(first_name = substr(first_name, 1, 1),
         player_fuzzy = paste(first_name, "", last_name),
         player_name_first = word(player_fuzzy, 1),
         player_name_last = word(player_fuzzy, -1),
         player_name_last = if_else(player_name_last == "", last_name, player_name_last),
         player_fuzzy = paste(player_name_first, "", player_name_last)) %>%
  select(player, player_fuzzy, everything(), -first_name, -last_name) %>%
  stringdist_left_join(ipl_auction_2023, by = c('player_fuzzy' = 'player_fuzzy', 'merge_team'='merge_team'), max_dist = 3, 
                       distance_col = "distance") %>%
  group_by(player) %>%
  slice_min(player_fuzzy.distance) %>%
  ungroup() %>%
  mutate(wpa_per_thous = total_wpa/(price_dollars_thousands),
         wpa_per_thous = round(wpa_per_thous, digits = 2),
         wpa_per_lakh = total_wpa/(price_lakhs),
         wpa_per_lakh = round(wpa_per_lakh, digits = 2),
         XRA_per_thous = total_XRA/(price_dollars_thousands),
         XRA_per_thous = round(XRA_per_thous, digits = 2),
         XRA_per_lakh = total_XRA/(price_lakhs),
         XRA_per_lakh = round(XRA_per_lakh, digits = 2)) %>%
  select(-player_fuzzy.x, -player_fuzzy.y, -merge_team.x, -merge_team.y, -distance, -player_fuzzy.distance, -merge_team.distance) %>%
  drop_na(player_full)


# Google Upload -----------------------------------------------------------



write.csv(wpa_ipl_2023, "wpa_ipl_2023.csv")
drive_upload("wpa_ipl_2023.csv", name = "wpa_ipl_2023", type = "spreadsheet", overwrite = TRUE)
1
# Salary Plots ------------------------------------------------------------

col <- as.character(wpa_ipl_2023$team_colours)
names(col) <- as.character(wpa_ipl_2023$team)
col2 <- as.character(wpa_ipl_2023$team_colour2)
names(col2) <- as.character(wpa_ipl_2023$team)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.9*a", channel = "alpha")
}

wpa_ipl_2023 %>%
  mutate(price_dollars_millions = price_dollars_thousands/1000) %>%
  ggplot(aes(x = price_dollars_millions, y = total_wpa)) +
  geom_point(aes(colour = position_detail)) +
  geom_smooth(aes(colour = position_detail), se = FALSE) +
  theme_gdocs() +
  scale_colour_hc() +
  labs(title = "Indian Premier League 2023 Contract amounts do not seem correlated with Win Probability Added",
       caption = "Chart: @TAlbTree. Data: cricketdata & espncricinfo",
       x = "USD$ Millions",
       y = "Total Win Probability Added",
       colour = "Position") +
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10))

ggsave(filename = glue("ment20/scatter_ipl_2023_wpa_vs_salary.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 4)

wpa_ipl_2023 %>%
  mutate(price_dollars_millions = price_dollars_thousands/1000) %>%
  ggplot(aes(x = as.factor(position_detail), y = price_dollars_millions)) +
  geom_beeswarm(aes(fill = position_detail, colour = position_detail), size = 3) +
  theme_gdocs() +
  scale_fill_hc() +
  scale_colour_hc() +
  labs(title = "Indian Premier League 2023 Salaries by Position",
       caption = "Chart: @TAlbTree. Data: cricketdata & espncricinfo",
       x = "",
       y = "USD$ Millions") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10))
ggsave(filename = glue("ment20/beeswarm_ipl_2023_salaries_by_position.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 5)

wpa_ipl_2023 %>%
  group_by(team, logo, team_colours, team_colour2) %>%
  summarise(total_wpa = sum(total_wpa),
            total_XRa = sum(total_XRA),
            total_salary_millions = sum(price_dollars_thousands/1000)) %>%
  ungroup() %>%
  ggplot(aes(x = total_salary_millions, y = total_wpa)) +
  geom_image(aes(image = logo, y = total_wpa, x = total_salary_millions), 
             image_fun = transparent, asp = 2, size = 0.08) +
  ylim(-600, 600)+
  theme_gdocs() +
  labs(title = "Gujarat Titans finished as Runners up in the 2023 Indian Premier League",
       subtitle = "but they were the Moneyball champions in our hearts",
       caption = "Chart: @TAlbTree. Data: cricketdata & espncricinfo",
       x = "USD$ Millions - Total Payroll",
       y = "Total Win Probability Added") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10))
ggsave(filename = glue("ment20/image_ipl_2023_salaries_vs_WPA.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 5)

wpa_ipl_2023 %>%
  group_by(team, logo, team_colours, team_colour2) %>%
  summarise(total_wpa = sum(total_wpa),
            total_XRA = sum(total_XRA),
            total_salary_millions = sum(price_dollars_thousands/1000)) %>%
  ungroup() %>%
  ggplot(aes(x = total_salary_millions, y = total_XRA)) +
  geom_image(aes(image = logo, y = total_XRA, x = total_salary_millions), 
             image_fun = transparent, asp = 2, size = 0.08) +
  ylim(-600, 600)+
  theme_gdocs() +
  labs(title = "Gujarat Titans finished as Runners up in the 2023 Indian Premier League",
       subtitle = "but they were the Moneyball champions in our hearts",
       caption = "Chart: @TAlbTree. Data: cricketdata & espncricinfo",
       x = "USD$ Millions - Total Payroll",
       y = "Total Win Probability Added") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10))
ggsave(filename = glue("ment20/image_ipl_2023_salaries_vs_WPA.png"), bg = "#ffffff",
       dpi = 1000, width = 10, height = 5)

wpa_ipl_2023 %>%
  mutate(price_dollars = price_dollars_thousands*1000) %>%
  filter(balls_total >= 100) %>%
  dplyr::select(player_full, logo, position, price_dollars, wpa_per_thous, total_wpa, balls_total) %>%
  arrange(desc(wpa_per_thous)) %>%
  slice_max(wpa_per_thous, n = 10) %>%
  gt() %>%
  tab_header(
    title = "The Top 10 Most Overperforming Contracts in the 2023 Men's Indian Premier League"
  ) %>%
  cols_align(
    "center",
    columns = c(player_full, logo, position, price_dollars, wpa_per_thous, total_wpa, balls_total)) %>% 
  cols_label(
    player_full = "Player",
    logo = "Team",
    position = "Position",
    price_dollars = "Contract ($USD)",
    wpa_per_thous = "WPA/$1000",
    total_wpa = "Total WPA",
    balls_total = "Balls") %>%
  tab_source_note(
    source_note = "Minimum combined balls bowled or faced >= 100"
  ) %>%
  tab_source_note(
    source_note = "WPA = Win Probability Added"
  ) %>%
  tab_source_note(
    source_note = "Table: @TAlbTree"
  ) %>%
  tab_source_note(
    source_note = "Data: cricketdata & espncricinfo"
  ) %>%
  gt_img_rows(columns = logo, height = 50) %>%
  gt_hulk_col_numeric(price_dollars:total_wpa) %>%
  tab_options(footnotes.font.size = 12)%>%
gtsave("ment20/IPL_Top10valuedcontracts.png")

wpa_ipl_2023 %>%
  mutate(price_dollars = price_dollars_thousands*1000) %>%
  filter(balls_total >= 100) %>%
  dplyr::select(player_full, logo, position, price_dollars, wpa_per_thous, total_wpa, balls_total) %>%
  arrange(desc(wpa_per_thous)) %>%
  slice_min(wpa_per_thous, n = 10) %>%
  gt() %>%
  tab_header(
    title = "The Top 10 Most Underperforming Contracts in the 2023 Men's Indian Premier League"
  ) %>%
  cols_align(
    "center",
    columns = c(player_full, logo, position, price_dollars, wpa_per_thous, total_wpa, balls_total)) %>% 
  cols_label(
    player_full = "Player",
    logo = "Team",
    position = "Position",
    price_dollars = "Contract ($USD)",
    wpa_per_thous = "WPA/$1000",
    total_wpa = "Total WPA",
    balls_total = "Balls") %>%
  tab_source_note(
    source_note = "Minimum combined balls bowled or faced >= 100"
  ) %>%
  tab_source_note(
    source_note = "WPA = Win Probability Added"
  ) %>%
  tab_source_note(
    source_note = "Table: @TAlbTree"
  ) %>%
  tab_source_note(
    source_note = "Data: cricketdata & espncricinfo"
  ) %>%
  gt_img_rows(columns = logo, height = 50) %>%
  gt_hulk_col_numeric(price_dollars:total_wpa) %>%
  tab_options(footnotes.font.size = 12)%>%
  gtsave("ment20/IPL_Top10overvaluedcontracts.png")


wpa_ipl_2023 %>%
  mutate(price_dollars = price_dollars_thousands*1000) %>%
  dplyr::select(player_full, logo, position, price_dollars, wpa_per_thous, total_wpa, balls_total) %>%
  arrange(desc(price_dollars)) %>%
  slice_max(price_dollars, n = 10) %>%
  gt() %>%
  tab_header(
    title = "The Top 10 Most Expensive Contracts in the 2023 Men's Indian Premier League",
    subtitle = "They all did pretty poorly for their contracts..."
  ) %>%
  cols_align(
    "center",
    columns = c(player_full, logo, position, price_dollars, wpa_per_thous, total_wpa, balls_total)) %>% 
  cols_label(
    player_full = "Player",
    logo = "Team",
    position = "Position",
    price_dollars = "Contract ($USD)",
    wpa_per_thous = "WPA/$1000",
    total_wpa = "Total WPA",
    balls_total = "Balls") %>%
  tab_source_note(
    source_note = "WPA = Win Probability Added"
  ) %>%
  tab_source_note(
    source_note = "Table: @TAlbTree"
  ) %>%
  tab_source_note(
    source_note = "Data: cricketdata & espncricinfo"
  ) %>%
  gt_img_rows(columns = logo, height = 50) %>%
  gt_hulk_col_numeric(price_dollars:total_wpa) %>%
  tab_options(footnotes.font.size = 12)%>%
  gtsave("ment20/IPL_Top10highestcontracts.png")
