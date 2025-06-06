library(tidyverse)
mlb_batted_balls <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/mlb_batted_balls.csv")
view(mlb_batted_balls)

mlb_batted_balls |>
  filter(!is.na(strikes), !is.na(pitch_type)) |>
  mutate(strike_context = if_else(strikes == 2, "2 Strikes", "Other Counts")) |>
  mutate(pitch_group = case_when(
    pitch_type %in% c("CH", "CU", "FS", "SL", "KC", "KN", "EP", "SC", "CS", "SV", "FO", "ST") ~ "Offspeed",
    pitch_type %in% c("FC", "FF", "FT", "FA", "SI") ~ "Fastball"
    )) |>
  count(strike_context, pitch_group) |>
  ggplot(aes(x = strike_context, y = n, fill = pitch_group)) +
  geom_col(position = "fill")

# mlb_batted_balls |>
#   filter(!is.na(events), !is.na(if_fielding_alignment)) |>
#   count(if_fielding_alignment, events) |>
#   group_by(if_fielding_alignment) |>
#   ggplot(aes(x = if_fielding_alignment, y = n, fill = events)) +
#   geom_col(position = "fill") +
#   scale_fill_viridis_d(option = "plasma")


mlb_batted_balls |>
  filter(!is.na(events), !is.na(if_fielding_alignment)) |>
  mutate(event_group = case_when(
    events %in% c("single", "double", "triple", "home_run", "sac_bunt", "sac_fly") ~ "Offensive Success",
    events %in% c("field_out", "fielders_choice", "fielders_choice_out", "force_out") ~ "1 Out",
    events %in% c("grounded_into_double_play", "sac_fly_double_play", "triple_play") ~ "2+ Outs"
  )) |>
  count(if_fielding_alignment, event_group) |>
  group_by(if_fielding_alignment) |>
  ggplot(aes(x = if_fielding_alignment, y = n, fill = event_group)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Proportion of Outcomes by Infield Shift",
       x = "Infield Shift Type",
       y = "Proportion",
       fill = "Event Group") +
  theme_minimal()

mlb_batted_balls |>
  filter(!is.na(events), inning >= 8, abs(home_score - away_score) <= 1, events %in% c("home_run", "triple", "double")) |>
  count(batter_name, sort = TRUE) |>
  slice_head(n = 10) |>
  ggplot(aes(x = reorder(batter_name, n), y = n)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(
    title = "Leaders in XBH in Clutch Situations in the 2024 Season",
    subtitle = "Clutch = 8th Inning or Later & Score Difference ≤ 1",
    x = "Batter",
    y = "XBHs"
  ) +
  scale_y_continuous(breaks = seq(0, 12, by = 1)) +
  theme_minimal()

mlb_batted_balls |>
  filter(!is.na(launch_speed)) |>
  group_by(batter_name) |>
  summarise(
    avg_launch_speed = mean(launch_speed),
    n = n()
  ) |>
  arrange(desc(avg_launch_speed))

