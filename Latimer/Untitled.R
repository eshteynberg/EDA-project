library(tidyverse)
mlb_batted_balls <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/mlb_batted_balls.csv")
view(mlb_batted_balls)

mlb_batted_balls |>
  count(pitch_type, if_fielding_alignment) |>
  ggplot(aes(x = if_fielding_alignment, y = n, 
             # filled by the other categorical variable
             fill = pitch_type)) + 
  geom_col()
