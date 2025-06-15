library(tidyverse)
library(ggrepel)
mlb_batted_balls <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/mlb_batted_balls.csv")
view(mlb_batted_balls)
library(sportyR)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(shiny)
library(plotly)

# Same vs. Opposite Handedness
# We found little to no difference in the
# proportion of outcomes whether the batter
# was the same or opposite handedness as the
# pitcher. This is very interesting because
# in baseball, handedness match ups are seen
# to be very important.
# When making a decision on batting / pitching
# changes, it is much more important to look at
# what players perform better against that side,
# rather than purely relying on opposite / same
# handedness match ups.

mlb_batted_balls |>
  filter(!is.na(events), !is.na(pitch_type)) |>
  mutate(event_group = case_when(
    events == "single" ~ "Single",
    events == "double" ~ "Double",
    events == "triple" ~ "Triple",
    events == "home_run" ~ "Home Run",
    events %in% c("field_out", "fielders_choice", "fielders_choice_out", "force_out", "sac_bunt", "sac_fly") ~ "1 Out",
    events %in% c("grounded_into_double_play", "sac_fly_double_play", "triple_play", "double_play") ~ "2+ Outs",
    events %in% c("field_error") ~ "Error"
  )) |>
  mutate(event_group = factor(event_group, levels = c("Error", "2+ Outs", "1 Out", "Home Run", "Triple", "Double", "Single"))) |>
  count(pitch_type, event_group) |>
  ggplot(aes(x = pitch_type, y = n, fill = event_group)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_col(position = "fill")

# mlb_batted_balls |>
#   filter(!is.na(events), !is.na(pitch_type)) |>
#   mutate(event_group = case_when(
#     events == "single" ~ "Single",
#     events == "double" ~ "Double",
#     events == "triple" ~ "Triple",
#     events == "home_run" ~ "Home Run",
#     events %in% c("field_out", "fielders_choice", "fielders_choice_out", "force_out", "sac_bunt", "sac_fly") ~ "1 Out",
#     events %in% c("grounded_into_double_play", "sac_fly_double_play", "triple_play", "double_play") ~ "2+ Outs",
#     events %in% c("field_error") ~ "Error"
#   )) |>
#   mutate(event_group = factor(event_group, levels = c("Error", "2+ Outs", "1 Out", "Home Run", "Triple", "Double", "Single"))) |>
#   mutate(side = case_when(
#     (bat_side == "R" & pitch_hand == "R") | (bat_side == "L" & pitch_hand == "L") ~ "Same Side",
#     (bat_side == "L" & pitch_hand == "R") | (bat_side == "R" & pitch_hand == "L") ~ "Opposite Side"
#     )) |>
#   filter(!is.na(side)) |>
#   group_by(side) |>
#   summarize(
#     n_batted_balls = n(),
#     n_home_runs = sum(events == "home_run"),
#     hr_rate = n_home_runs / n_batted_balls
#   ) |>
#   ggplot(aes(x = side, y = hr_rate, fill = side)) +
#   geom_col(show.legend = FALSE) +
#   scale_fill_viridis_d() +
#   labs(
#     x = "Pitcher/Batter Handedness Matchup",
#     y = "Home Run Rate",
#     title = "Home Run Rate by Pitcher/Batter Handedness"
#   ) +
#   theme_minimal()

mlb_batted_balls |>
  filter(!is.na(events), !is.na(pitch_type), !is.na(bat_side), !is.na(pitch_hand)) |>
  mutate(event_group = case_when(
    events == "single" ~ "Single",
    events == "double" ~ "Double",
    events == "triple" ~ "Triple",
    events == "home_run" ~ "Home Run",
    events %in% c("field_out", "fielders_choice", "fielders_choice_out", "force_out", "sac_bunt", "sac_fly") ~ "1 Out",
    events %in% c("grounded_into_double_play", "sac_fly_double_play", "triple_play", "double_play") ~ "2+ Outs",
    events %in% c("field_error") ~ "Error",
    TRUE ~ NA_character_
  )) |>
  mutate(event_group = factor(event_group, levels = c("Error", "2+ Outs", "1 Out", "Home Run", "Triple", "Double", "Single"))) |>
  mutate(side = case_when(
    (bat_side == "R" & pitch_hand == "R") | (bat_side == "L" & pitch_hand == "L") ~ "Same Side",
    (bat_side == "L" & pitch_hand == "R") | (bat_side == "R" & pitch_hand == "L") ~ "Opposite Side"
  )) |>
  filter(!is.na(event_group), !is.na(side)) |>
  count(side, event_group) |>
  ggplot(aes(x = side, y = n, fill = event_group)) +
  geom_col(position = "fill") +
  scale_fill_viridis_d() +
  labs(
    x = "Pitcher/Batter Handedness Matchup",
    y = "Proportion of Outcomes",
    fill = "Outcome",
    title = "Batted Ball Outcomes by Handedness Matchup"
  ) +
  theme_minimal()




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
    events == "single" ~ "Single",
    events == "double" ~ "Double",
    events == "triple" ~ "Triple",
    events == "home_run" ~ "Home Run",
    events %in% c("field_out", "fielders_choice", "fielders_choice_out", "force_out", "sac_bunt", "sac_fly") ~ "1 Out",
    events %in% c("grounded_into_double_play", "sac_fly_double_play", "triple_play", "double_play") ~ "2+ Outs",
    events %in% c("field_error") ~ "Error"
  )) |>
  mutate(event_group = factor(event_group, levels = c("Error", "2+ Outs", "1 Out", "Home Run", "Triple", "Double", "Single"))) |>
  count(if_fielding_alignment, event_group) |>
  group_by(if_fielding_alignment) |>
  ggplot(aes(x = if_fielding_alignment, y = n, fill = event_group)) +
  geom_col(position = "fill") +
  labs(title = "Proportion of Outcomes by Infield Shift",
       x = "Infield Shift Type",
       y = "Proportion",
       fill = "Event Group") +
  scale_fill_viridis(discrete = TRUE) +
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
  filter(!is.na(events), !is.na(bat_side), !is.na(pitch_hand)) |>
  mutate(side = case_when(
    (bat_side == "R" & pitch_hand == "R") | (bat_side == "L" & pitch_hand == "L") ~ "SameSide",
    (bat_side == "L" & pitch_hand == "R") | (bat_side == "R" & pitch_hand == "L") ~ "OppositeSide"
  )) |>
  group_by(batter_name) |>
  summarise(
    bip_value_same = (
      sum(events == "single" & side == "SameSide", na.rm = TRUE) * 1 +
        sum(events == "double" & side == "SameSide", na.rm = TRUE) * 2 +
        sum(events == "triple" & side == "SameSide", na.rm = TRUE) * 3 +
        sum(events == "home_run" & side == "SameSide", na.rm = TRUE) * 4
    ) / sum(side == "SameSide", na.rm = TRUE),
    
    bip_value_oppo = (
      sum(events == "single" & side == "OppositeSide", na.rm = TRUE) * 1 +
        sum(events == "double" & side == "OppositeSide", na.rm = TRUE) * 2 +
        sum(events == "triple" & side == "OppositeSide", na.rm = TRUE) * 3 +
        sum(events == "home_run" & side == "OppositeSide", na.rm = TRUE) * 4
    ) / sum(side == "OppositeSide", na.rm = TRUE),
    
    n_same = sum(side == "SameSide", na.rm = TRUE),
    n_oppo = sum(side == "OppositeSide", na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n_same >= 20, n_oppo >= 20) |>
  mutate(bip_advantage = case_when(
    bip_value_same > bip_value_oppo ~ "Same-Side Advantage",
    bip_value_same < bip_value_oppo ~ "Opposite-Side Advantage",
    TRUE ~ "No Difference"
  )) |>
  ggplot(aes(x = bip_value_oppo, y = bip_value_same)) +
  geom_point(aes(color = bip_advantage), alpha = 0.7) +
  scale_color_manual(values = c("red", "#117733", "#3399FF")) +
  geom_smooth(method = "lm", color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "BIP Value vs Opposite-Side Pitching",
    y = "BIP Value vs Same-Side Pitching",
    title = "Player BIP Value Split by Pitcher Handedness",
    subtitle = "Players with at least 20 batted balls per side",
    color = "BIP Advantage"
  ) +
  coord_fixed() +
  theme_minimal()


# glimpse(mlb_batted_balls)
# 
# cor(mlb_batted_balls$release_speed, 
#     mlb_batted_balls$effective_speed, 
#     use = "complete.obs")


# Spray Chart
# install.packages("ggbaseball")

mlb_batted_balls_filtered <- mlb_batted_balls |>
  filter(!is.na(hit_coord_x), !is.na(hit_coord_y), !is.na(events)) |>
  mutate(location_x = 2.5 * (hit_coord_x - 125.42),
         location_y = 2.5 * (198.27 - hit_coord_y),
         distance = sqrt(location_x ** 2 + location_y ** 2)) |>
  filter(balls == 3, strikes == 0)

geom_baseball(league = "MLB") +
  geom_point(data = mlb_batted_balls_filtered, aes(location_x, location_y, color = events, alpha = 0.1))
  

# mlb_batted_balls |>
#   filter(!is.na(balls), !is.na(strikes), events == "home_run") |>
#   group_by(strikes, balls) |>
#   summarize(hr_count = n(), .groups = "drop" ) |>
#   ggplot(aes(x = factor(strikes), y = hr_count, fill = factor(balls))) +
#   geom_col(position = "fill") +
#   labs(x = "Strikes", y = "Home Runs", fill = "Balls")

# Proportion of Outcomes by Pitch Bar Chart

count_prop <- mlb_batted_balls |>
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
  mutate(event_group = factor(event_group, levels = c("2+ Outs", "1 Out", "Error", "Home Run", "Triple", "Double", "Single"))) |>
  mutate(counts = forcats::fct_rev(factor(counts))) |>
  count(counts, event_group) |>
  ggplot(aes(x = counts, y = n, fill = event_group)) +
  geom_col(position = "fill") +
  labs(y = "Proportion of Outcomes", x = "Counts (Balls-Strikes)", title = "Proportion of Outcomes in Certain Counts", fill = "Outcome Type") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal()

count_prop

ggsave("Count_prop.png", plot = count_prop, width = 8, height = 6, dpi = 300)


mlb_batted_balls |>
  filter(!is.na(pitch_number), !is.na(events)) |>
  group_by(pitch_number, events) |>
  summarize(n = n(), .groups = "drop") |>
  ggplot(aes(x = pitch_number, y = n, fill = events)) +
  geom_col(position = "fill") +
  scale_fill_viridis(discrete = TRUE) +
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
      y = ~bip_value,
      x = ~avg_swing_len,
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
        xaxis = list(title = "Average Swing Length"),
        yaxis = list(title = "BiP Value"),
        colorbar = list(title = "Average Exit Velocity (mph)")
      )
  })
}

shinyApp(ui = ui, server = server)
  

set.seed(42)

mlb_features <- mlb_batted_balls |>
  group_by(batter_name) |>
  summarize(
    bip_value = (
      sum(events == "single", na.rm = TRUE) * 1 +
        sum(events == "double", na.rm = TRUE) * 2 +
        sum(events == "triple", na.rm = TRUE) * 3 +
        sum(events == "home_run", na.rm = TRUE) * 4
    ) / n(),
    avg_launch_speed = mean(launch_speed, na.rm = TRUE),
    avg_launch_angle = mean(launch_angle, na.rm = TRUE),
    avg_bat_speed = mean(bat_speed, na.rm = TRUE),
    balls_in_play = n()
  ) |>
  filter(balls_in_play >= 50) |>
  select(-balls_in_play) |>
  drop_na()
  
features_scaled <- mlb_features |>
  select(-batter_name) |>
  scale()

k_result <- features_scaled |>
  kmeans(centers = 4, nstart = 100)

library(factoextra)

k_result |>
  fviz_cluster(data = features_scaled,
               geom = "point",
               ellipse = FALSE) +
  ggthemes::scale_color_colorblind() + 
  theme_light()
  
mlb_summary2_with_pca <- pca_scores |>
  left_join(mlb_features |> select(batter_name, avg_launch_speed, avg_launch_angle, avg_bat_speed), by = "batter_name")

mlb_summary2_with_pca |>
  mutate(
    player_cluster = as.factor(cluster)
  ) |>
  ggplot(aes(x = avg_bat_speed, y = avg_launch_angle, color = player_cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  ggthemes::scale_color_colorblind() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    x = "Average Bat Speed",
    y = "Average Launch Angle",
    color = "Cluster",
    title = "MLB Player Clusters Based on Batted Ball Profile"
  )
  
set.seed(42)
mlb_features <- mlb_batted_balls |>
  group_by(batter_name) |> 
  summarize(
    bip_value = (
      sum(events == "single", na.rm = TRUE) * 1 +
        sum(events == "double", na.rm = TRUE) * 2 +
        sum(events == "triple", na.rm = TRUE) * 3 +
        sum(events == "home_run", na.rm = TRUE) * 4
    ) / n(),
    avg_launch_speed=mean(launch_speed, na.rm=TRUE),
    avg_launch_angle=mean(launch_angle, na.rm=TRUE),
    avg_bat_speed=mean(bat_speed, na.rm=TRUE),
    balls_in_play=n()
  ) |>
  filter(balls_in_play>=50) |> 
  select(-balls_in_play) |> 
  drop_na()

std_mlb_features <-mlb_features |> 
  select(-batter_name) |> 
  scale()

#default kmeans (Hartigan-Wong)
kmeans_result<- std_mlb_features |> 
  kmeans(centers=3, nstart=100)

mlb_clusters <- data.frame(
  batter_name = mlb_features$batter_name,
  cluster=kmeans_result$cluster
)




#to visualize results with many dimensions (features), do PCA and plot
#plot top 2 dimensions (capture most of the variation in the data)
library(factoextra)

kmeans_result |> 
  fviz_cluster(data=std_mlb_features,
               geom="point",
               ellipse=FALSE)+
  ggthemes::scale_color_colorblind()+
  theme_minimal()

pca_result <- prcomp(std_mlb_features, center=TRUE, scale.=TRUE)
summary(pca_result)
pca_result$rotation

std_mlb_features_df <- as.data.frame(mlb_features) |>
  mutate(
    batter_name = mlb_features$batter_name,
    cluster = k_result$cluster
  )

std_mlb_features_df |>
  mutate(
    player_cluster = as.factor(cluster),
    label_player = ifelse(avg_launch_angle < 10 & avg_launch_speed < 85, batter_name, NA)
  ) |>
  ggplot(aes(x = avg_launch_speed, y = avg_launch_angle, color = player_cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text_repel(aes(label = label_player), size = 3, max.overlaps = 15) +
  ggthemes::scale_color_colorblind() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    x = "Average Launch Speed",
    y = "Average Launch Angle",
    color = "Cluster",
    title = "MLB Player Clusters Based on Batted Ball Profile"
  )

std_mlb_features_df |>
  group_by(cluster) |>
  summarise(
    avg_bip_value = mean(bip_value, na.rm = TRUE),
    count = n()
  )




  
# ui <- fluidPage(
#   titlePanel("Player Archetypes - PCA"),
#   plotlyOutput("scatterPlot")
# )
#   
# server <- function(input, output) {
#   output$scatterPlot <- renderPlotly({
#     plot_ly(
#       data = mlb_summary2_with_pca,
#       x = ~PC1,
#       y = ~PC2,
#       color = ~cluster,
#       colors = c("black", "gold", "lightblue"),
#       type = 'scatter',
#       mode = 'markers',
#       text = ~paste0(
#         batter_name, "<br>",
#         "Cluster: ", cluster, "<br>",
#         "HR Rate: ", round(hr_rate, 3), "<br>",
#         "BA on Batted Balls: ", round(bip_hit_pct, 3), "<br>",
#         "Avg EV: ", round(avg_launch_speed, 3), " mph"
#       ),
#       hoverinfo = 'text',
#       marker = list(size = 10, opacity = 0.75)
#     ) |>
#     layout(
#       xaxis = list(title = "PC1: Power Hitting (BiP Value, HR Rate, etc.)"),
#       yaxis = list(title = "PC2: Contact Hitting (Avg Launch Angle, Hit %, etc.)")
#     )
#   })
# }
# 
# shinyApp(ui = ui, server = server)
