library(tidyverse)
library(ggrepel)
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
  ggplot(aes(x = n, y = avg_launch_speed)) +
  geom_point(alpha = 0.2) +
  #geom_text(aes(label=ifelse(avg_launch_speed>93.6 & n > 250,as.character(batter_name), ''))) +
  geom_text_repel(aes(label=ifelse((avg_launch_speed>93.6 & n > 250) | avg_launch_speed > 105,as.character(batter_name), '')))

install.packages("palmerpenguins")

glimpse(mlb_batted_balls)

cor(mlb_batted_balls$release_speed, 
    mlb_batted_balls$effective_speed, 
    use = "complete.obs")

mlb_batted_balls |>
  filter(!is.na(hit_coord_x), !is.na(hit_coord_y), !is.na(events), batter_name == "Devers, Rafael") |>
  group_by(events) |>
  ggplot(aes(x = hit_coord_x, y = -1 *hit_coord_y)) +
  geom_point(aes(color = events, alpha=0.001))

mlb_batted_balls |>
  filter(!is.na(balls), !is.na(strikes), !is.na(events)) |>
  group_by(events) |>
  summarize(hr = )
  ggplot(aes(x = strikes, y = balls)) +
  geom_tile(aes(fill = events))

library(shiny)
library(plotly)
library(dplyr)

  mlb_summary <- mlb_batted_balls |>
    group_by(batter_name) |>
    summarize(
      avg_launch_speed = mean(launch_speed, na.rm = TRUE),
      bip_value = (
        sum(events == "single", na.rm = TRUE) * 1 +
          sum(events == "double", na.rm = TRUE) * 2 +
          sum(events == "triple", na.rm = TRUE) * 3 +
          sum(events == "home_run", na.rm = TRUE) * 4
      ) / n(),
      line_drive_pct = sum(bb_type == "line_drive", na.rm = TRUE) / n() * 100,
      balls_in_play = n(),
      bip_hit_pct = (sum(events == "single", na.rm = TRUE) +
        sum(events == "double", na.rm = TRUE) +
        sum(events == "triple", na.rm = TRUE) +
        sum(events == "home_run", na.rm = TRUE)
    ) / n()
    ) |>
    filter(balls_in_play > 150) |>
    ungroup()

  ui <- fluidPage(
    titlePanel("Quality of Batted Balls by Player"),
    plotlyOutput("scatterPlot")
  )

  server <- function(input, output) {
    output$scatterPlot <- renderPlotly({
      plot_ly(
        data = mlb_summary,
        x = ~bip_hit_pct,
        y = ~bip_value,
        type = 'scatter',
        mode = 'markers',
        color = ~avg_launch_speed,                 # color scale based on home runs
        colors = "viridis",
        text = ~paste0(
          batter_name, "<br>",
          "Avg Hit Value: ", round(bip_value, 3), "<br>",
          "BA on Batted Balls: ", round(bip_hit_pct, 3), "<br>",
          "Avg EV: ", round(avg_launch_speed, 3), " mph"
        ),
        hoverinfo = 'text',
        marker = list(size = 10, opacity = 0.75)
      ) %>%
        layout(
          xaxis = list(title = "Batting Average on Batted Balls"),
          yaxis = list(title = "Value of Hits"),
          colorbar = list(title = "Average Exit Velocity (mph)")
        )
    })
  }

  
  shinyApp(ui = ui, server = server)

  
  
  
  