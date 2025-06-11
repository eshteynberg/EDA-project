library(tidyverse)
library(ggrepel)
mlb_batted_balls <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/mlb_batted_balls.csv")
view(mlb_batted_balls)

mlb_batted_balls |>
  filter(!is.na(events), !is.na(pitch_type)) |>
  mutate(event_group = case_when(
    events %in% c("single", "double", "triple", "home_run") ~ "Offensive Success",
    events %in% c("field_out", "fielders_choice", "fielders_choice_out", "force_out", "sac_bunt", "sac_fly") ~ "1 Out",
    events %in% c("grounded_into_double_play", "sac_fly_double_play", "triple_play", "double_play") ~ "2+ Outs",
    events %in% c("field_error") ~ "Error"
  )) |>
  count(pitch_type, event_group) |>
  ggplot(aes(x = pitch_type, y = n, fill = event_group)) +
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
    events %in% c("grounded_into_double_play", "sac_fly_double_play", "triple_play", "double_play") ~ "2+ Outs",
    events %in% c("field_error") ~ "Error"
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
    subtitle = "Clutch = 8th Inning or Later & Score Differential ≤ 1",
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
  filter(!is.na(balls), !is.na(strikes), events == "home_run") |>
  group_by(strikes, balls) |>
  summarize(hr_count = n(), .groups = "drop" ) |>
  ggplot(aes(x = factor(strikes), y = hr_count, fill = factor(balls))) +
  geom_col(position = "dodge") +
  labs(x = "Strikes", y = "Home Runs", fill = "Balls")

library(dplyr)
library(ggplot2)
library(RColorBrewer)

mlb_batted_balls |>
  filter(!is.na(balls), !is.na(strikes), !is.na(events), events != "field_error") |>
  mutate(counts = paste0(balls, "-", strikes)) |>
  mutate(event_group = case_when(
    events == "single" ~ "Single",
    events == "double" ~ "Double",
    events == "triple" ~ "Triple",
    events == "home_run" ~ "Home Run",
    events %in% c("field_out", "fielders_choice", "fielders_choice_out", "force_out", "sac_bunt", "sac_fly") ~ "1 Out",
    events %in% c("grounded_into_double_play", "sac_fly_double_play", "triple_play", "double_play") ~ "2+ Outs",
    events %in% c("field_error") ~ "Error"
  )) |>
  count(counts, event_group) |>
  ggplot(aes(x = counts, y = n, fill = event_group)) +
  geom_col(position = "fill") +
  ggthemes::scale_fill_colorblind()

mlb_batted_balls |>
  filter(!is.na(pitch_number), !is.na(events)) |>
  group_by(pitch_number, events) |>
  summarize(n = n(), .groups = "drop") |>
  ggplot(aes(x = pitch_number, y = n, fill = events)) +
  geom_col(position = "fill") +
  labs(x = "Pitch Number", y = "Count", title = "Batted Ball Events by Pitch Number")

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
    hr_rate = sum(events == "home_run") / n(),
    line_drive_pct = sum(bb_type == "line_drive", na.rm = TRUE) / n() * 100,
    balls_in_play = n(),
    bip_hit_pct = (sum(events == "single", na.rm = TRUE) +
      sum(events == "double", na.rm = TRUE) +
      sum(events == "triple", na.rm = TRUE) +
      sum(events == "home_run", na.rm = TRUE)
  ) / n(),
    avg_launch_angle = mean(launch_angle, na.rm = TRUE)) |>
  filter(balls_in_play > 150) |>
  ungroup()

ui1 <- fluidPage(
  titlePanel("Quality of Batted Balls by Player"),
  plotlyOutput("scatterPlot")
)

server1 <- function(input, output) {
  output$scatterPlot <- renderPlotly({
    plot_ly(
      data = mlb_summary,
      y = ~bip_value,
      x = ~bip_hit_pct,
      type = 'scatter',
      mode = 'markers',
      color = ~avg_launch_speed,                 # color scale based on home runs
      colors = "viridis",
      text = ~paste0(
        batter_name, "<br>",
        "Avg Hit Value: ", round(hr_rate, 3), "<br>",
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

shinyApp(ui = ui1, server = server1)

# pitch number could be interesting; hypothesis: it is in the
# batter's best interest to extend the at-bat, not only because
# it's more pitches for the pitcher but it also results in better outcomes

# also look at counts: bar graph

mlb_summary2 <- mlb_batted_balls |>
  group_by(batter_name) |>
  summarize(
    avg_launch_speed = mean(launch_speed, na.rm = TRUE),
    bip_value = (
      sum(events == "single", na.rm = TRUE) * 1 +
        sum(events == "double", na.rm = TRUE) * 2 +
        sum(events == "triple", na.rm = TRUE) * 3 +
        sum(events == "home_run", na.rm = TRUE) * 4
    ) / n(),
    hr_rate = sum(events == "home_run") / n(),
    line_drive_pct = sum(bb_type == "line_drive", na.rm = TRUE) / n() * 100,
    balls_in_play = n(),
    bip_hit_pct = (sum(events == "single", na.rm = TRUE) +
                     sum(events == "double", na.rm = TRUE) +
                     sum(events == "triple", na.rm = TRUE) +
                     sum(events == "home_run", na.rm = TRUE)
    ) / n(),
    avg_launch_angle = mean(launch_angle, na.rm = TRUE),
    avg_bat_speed = mean(bat_speed, na.rm = TRUE),
    avg_swing_len = mean(swing_length, na.rm = TRUE),
    solid_hit_pct = sum(launch_angle >= 15 & launch_angle <= 35 & launch_speed >= 90, na.rm = TRUE) / n() * 100,
    singles = sum(events == "single") / n(),
    avg_strike_zone = mean(strike_zone_top, na.rm = TRUE) - mean(strike_zone_bottom, na.rm = TRUE)
    )|>
  filter(balls_in_play > 150) |>
  ungroup()

ui <- fluidPage(
  titlePanel("Player Archetypes"),
  plotlyOutput("scatterPlot")
)

server <- function(input, output) {
  output$scatterPlot <- renderPlotly({
    plot_ly(
      data = mlb_summary2,
      y = ~hr_rate,
      x = ~bip_hit_pct,
      type = 'scatter',
      mode = 'markers',
      color = ~avg_bat_speed,                 # color scale based on home runs
      colors = "viridis",
      text = ~paste0(
        batter_name, "<br>",
        "Avg Hit Value: ", round(hr_rate, 3), "<br>",
        "BA on Batted Balls: ", round(bip_hit_pct, 3), "<br>",
        "Avg EV: ", round(avg_launch_speed, 3), " mph"
      ),
      hoverinfo = 'text',
      marker = list(size = 10, opacity = 0.75)
    ) %>%
      layout(
        xaxis = list(title = "Average Swing Length"),
        yaxis = list(title = "Solid Hit %"),
        colorbar = list(title = "Average Exit Velocity (mph)")
      )
  })
}

shinyApp(ui = ui, server = server)
  
mlb_features <- mlb_batted_balls |>
  group_by(batter_name) |>
  summarize(
    avg_launch_speed = mean(launch_speed, na.rm = TRUE),
    avg_launch_angle = mean(launch_angle, na.rm = TRUE),
    avg_bat_speed = mean(bat_speed, na.rm = TRUE),
    hr_rate = sum(events == "home_run", na.rm = TRUE) / n(),
    bip_value = (
      sum(events == "single", na.rm = TRUE) * 1 +
      sum(events == "double", na.rm = TRUE) * 2 +
      sum(events == "triple", na.rm = TRUE) * 3 +        sum(events == "home_run", na.rm = TRUE) * 4
    ) / n(),
    bip_hit_pct = (
      sum(events %in% c("single", "double", "triple", "home_run"), na.rm = TRUE)
  ) / n(),
    balls_in_play = n()
) |>
  filter(balls_in_play >= 50) |>
  drop_na()
  
features_scaled <- mlb_features |> select(-batter_name, -balls_in_play) |> scale()
pca_result <- prcomp(features_scaled, scale. = TRUE)
pca_scores <- as.data.frame(pca_result$x) |>
  mutate(batter_name = mlb_features$batter_name)

pca_scores$PC2 <- -pca_scores$PC2
  
  
set.seed(42)
k_result <- kmeans(select(pca_scores, PC1, PC2), centers = 3, nstart = 25)
pca_scores$cluster <- factor(k_result$cluster)
  
mlb_summary2_with_pca <- pca_scores |>
  left_join(mlb_features |> select(batter_name, hr_rate, bip_hit_pct, avg_launch_speed), by = "batter_name")
  

  
ui <- fluidPage(
  titlePanel("Player Archetypes - PCA"),
  plotlyOutput("scatterPlot")
)
  
server <- function(input, output) {
  output$scatterPlot <- renderPlotly({
    plot_ly(
      data = mlb_summary2_with_pca,
      x = ~PC1,
      y = ~PC2,
      color = ~cluster,
      colors = c("black", "gold", "lightblue"),
      type = 'scatter',
      mode = 'markers',
      text = ~paste0(
        batter_name, "<br>",
        "Cluster: ", cluster, "<br>",
        "HR Rate: ", round(hr_rate, 3), "<br>",
        "BA on Batted Balls: ", round(bip_hit_pct, 3), "<br>",
        "Avg EV: ", round(avg_launch_speed, 3), " mph"
      ),
      hoverinfo = 'text',
      marker = list(size = 10, opacity = 0.75)
    ) |>
    layout(
      xaxis = list(title = "PC1: Power Hitting (BiP Value, HR Rate, etc.)"),
      yaxis = list(title = "PC2: Contact Hitting (Avg Launch Angle, Hit %, etc.)")
    )
  })
}

library(shiny)
library(plotly)

shinyApp(ui = ui, server = server)
