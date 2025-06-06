library(tidyverse)
mlb <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/mlb_batted_balls.csv")
View(mlb)
names(mlb)

mlb<- mlb |> 
  mutate(new_launch_speed = launch_speed*(1609.34/3600)) |> 
  mutate(rad_launch_angle = launch_angle*(pi/180)) |> 
  mutate(hang_time= 2*new_launch_speed*sin(launch_angle)/9.80665)
summary(mlb$hang_time)
summary(mlb$new_launch_speed)

names(mlb)
#scatter plot of balls, strikes, outs vs launch_speed

mlb |> 
  ggplot(aes(x=outs, y=launch_speed)) +
  geom_point(size=1,alpha=0.5) 

#find which players have highest avg launch speed
mlb |> 
  group_by(batter_name) |> 
  summarize(avg_launch_speed=mean(launch_speed)) |> 
  arrange(desc(avg_launch_speed))





