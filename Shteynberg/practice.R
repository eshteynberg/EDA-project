library(tidyverse)
mlb <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/mlb_batted_balls.csv")
View(mlb)
names(mlb)

#####QUESTION 1#####
#filter to exclude NA
mlb <- mlb |> 
  filter(!is.na(launch_speed), !is.na(launch_angle), !is.na(bat_speed),
         !is.na(swing_length), !is.na(events))

#boxplot of launch_speed by events
#how hard batter hits to achieve different outcomes
mlb |> ggplot(aes(x=events,y=launch_speed))+
  geom_boxplot(fill="skyblue")+
  labs(title="Launch Speed by Event", x="Event", y="Launch Speed (mph)")+
  theme_minimal()+
  coord_flip() #cuz names overlap on the bottom of plot otherwise

#####

#heatmap of launch_speed vs launch_angle (proportion of hits)
#where hits cluster in terms of exit velocity and angle (barrel zone)
#first check range for binning later
range(mlb$lauch_speed)
range(mlb$launch_angle)

mlb |> 
  mutate(hit=ifelse(events %in% c("single", "double", "triple","home_run"), 1, 0),
         launch_speed_bin = cut(launch_speed, breaks=seq(10, 130, by=10)),
         launch_angle_bin=cut(launch_angle, breaks=seq(-90,90, by=10))) |> 
  #hit indicator (1 if hit, else 0)
  #binned launch speeds and launch angles into ranges
  #basically discrete buckets to group together similar batted balls
  group_by(launch_speed_bin, launch_angle_bin) |> 
  summarize(hit_rate = mean(hit, na.rm=TRUE)) |> 
  filter(!is.na(launch_speed_bin), !is.na(launch_angle_bin)) |> 
  ggplot(aes(x=launch_speed_bin, y=launch_angle_bin, fill=hit_rate)) +
  geom_tile(color="white")+
  scale_fill_gradient2(name="Hit Rate")+
  labs(title="Hit Rate by Launch Speed and Launch Angle",
       x="Launch Speeds",
       y="Launch Angles")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1), 
        panel.grid=element_blank(),
        axis.text = element_text(size=10),
        axis.title=element_text(size=12, face="bold"),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10))

  
#####
#scatterplot of bat_speed vs swing_length (color by bb_type)
mlb |> 
  filter(!is.na(bb_type), !is.na(swing_length), !is.na(bat_speed)) |> 
  ggplot(aes(x=swing_length, y=bat_speed, color=bb_type))+
  geom_point(alpha=0.5) +
  labs(title="Bat Speed vs Swing Length by Batted Ball Type",
       x="Swing Length (ft)",
       y="Bat speed (mph)",
       color="Batted Ball Type")+
  theme_minimal() +
  theme(axis.text=element_text(size=10),
        axis.title = element_text(size=12, face="bold"),
        legend.title= element_text(size=12),
        plot.title=element_text(size=14, face="bold", hjust=0.5))

#same thing but facet instead?
mlb |> 
  filter(!is.na(bb_type), !is.na(swing_length), !is.na(bat_speed)) |> 
  ggplot(aes(x=swing_length, y=bat_speed)) +
  geom_point(alpha=0.5, color="steelblue")+
  facet_wrap(~ bb_type, ncol=2)+
  labs(title = "Bat Speed vs Swing Length by Batted Ball Type",
       x="Swing Length (ft)",
       y="Bat Speed (mph)")+
  theme_minimal()+
  theme(axis.text=element_text(size=10),
        axis.title = element_text(size=12, face="bold"),
        plot.title=element_text(size=14, face="bold", hjust=0.5),
        strip.text=element_text(size=12, face="bold"))



#####QUESTION 2#####

#bar plot of avg launch speed by pitch type
mlb |> 
  filter(!is.na(pitch_type), !is.na(launch_speed)) |> 
  group_by(pitch_type) |> 
  summarize(avg_launch_speed = mean(launch_speed, na.rm=TRUE)) |> 
  ggplot(aes(x=reorder(pitch_type, -avg_launch_speed), y=avg_launch_speed))+
  geom_col(fill="coral")+
  labs(title="Average Launch Speed by Pitch Type",
       x="Pitch Type",
       y="Avg Launch Speed (mph)") +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        legend.position="none",
        plot.title=element_text(size=14, face="bold", hjust=0.5))
  
#Violin plot
#launch angle by pitcher handedness
#compare dists of launch angle between

mlb |> 
  mutate(handedness = ifelse(bat_side==pitch_hand, "Same side", "Opposite side")) |> 
  ggplot(aes(x=handedness,y=effective_speed)) +
  geom_violin()+
  geom_boxplot(width = 0.4) 


#########################################
mlb_feat <- mlb |> 
  select(where(is.numeric)) |> 
  na.omit()
mlb_pca <- prcomp(mlb_feat, center = TRUE, scale. = TRUE)
summary(mlb_pca)

mlb_pc_matrix <- mlb_pca$x
mlb_pca$rotation


mlb <- mlb |> 
  mutate(pc1 = mlb_pc_matrix[,1], 
         pc2 = mlb_pc_matrix[,2])
mlb |> 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point(alpha = 0.5) +
  labs(x = "PC 1", y = "PC 2")

library(factoextra)

mlb_pca |> 
  fviz_pca_biplot(label = "var",
                  alpha.ind = 0.25,
                  alpha.var = 0.75,
                  labelsize = 5,
                  col.var = "darkblue",
                  repel = TRUE)



mlb |> 
  filter(!is.na(balls), !is.na(outs), !is.na(events)) |> 
  ggplot(aes(x=swing_length, y=bat_speed, color=bb_type))+
  geom_point(alpha=0.5) +
  labs(title="Bat Speed vs Swing Length by Batted Ball Type",
       x="Swing Length (ft)",
       y="Bat speed (mph)",
       color="Batted Ball Type")+
  theme_minimal() +
  theme(axis.text=element_text(size=10),
        axis.title = element_text(size=12, face="bold"),
        legend.title= element_text(size=12),
        plot.title=element_text(size=14, face="bold", hjust=0.5))
###################################################

#spray chart
mlb |> 
  filter(!is.na(hit_coord_x), !is.na(hit_coord_y)) |> 
  ggplot(aes(x=hit_coord_x,y=-1*(hit_coord_y))) +
  geom_point(aes(color=events, alpha=0.5))




##################################################



#scatter plot of balls, strikes, outs vs launch_speed
mlb |> 
  ggplot(aes(x=outs, y=launch_speed)) +
  geom_point(size=1,alpha=0.5) 

#find which players have highest avg launch speed
mlb<- mlb |> 
  group_by(batter_name) |> 
  mutate(avg_launch_speed=mean(launch_speed)) 

mlb |> mutate(handedness = ifelse(bat_side==pitch_hand, "Same side", "Opposite side")) |> 
  ggplot(aes(x=avg_launch_speed, y=handedness)) +
  geom_boxplot()


#do count of home runs by handedness
table(mlb$events)
mlb |> 
  filter(events %in% "home_run") |> 
  ggplot()


########################################
#ANOTHER DRAFT

######Question 1#####
#How do swing mechanics relate to batted ball outcomes?
#What do swing length and bat speed tell us about quality or type of contact batter produces?
#Are there distinct swing profiles in MLB hitters?

#1A: Ridgeline plot (joyplot) - swing length dist by event
#Hypothesis: HR and flyers might require longer swings. Ground balls might not.
library(ggridges)
mlb |> 
  filter(!is.na(swing_length), !is.na(events)) |> 
  ggplot(aes(x=swing_length, y=events, fill=events))+
  geom_density_ridges(alpha=0.6)+
  theme_minimal()+
  labs(title="Swing Length Distribution by Batted Ball Outcome",
       x="Swing Length (ft)", y="Event")+
  theme(legend.position = "none")

#1B: K-means clustering? swing profiles? bat speed vs swing length scatterplot with clusters
#archetypes of hitters - short compact swings vs long power swings.

#####Question 2#####
#How do pitcher characteristics affect batted ball quality?



#####Question 3#####
#Where are balls typically hit, and how does spray pattern vary across context?
#Do certain swing styles or outcomes have typical spray tendencies?

#3A: scatterplot of hit_coord_x vs -hit_coord_y colored by event or batter


#3B: heatmap?

mlb |>
  filter(!is.na(launch_speed), !is.na(launch_angle), !is.na(bb_type)) |>
  ggplot(aes(x = launch_angle, y = launch_speed)) +
  geom_hex(bins = 30) +
  facet_wrap(~bb_type) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Launch Speed vs. Launch Angle by Batted Ball Type",
       x = "Launch Angle (°)", y = "Exit Velocity (mph)",
       fill = "Count") +
  theme_minimal()


#hexbin plot
mlb |>
  ggplot(aes(x = launch_speed, y = launch_angle)) +
  geom_hex(bins = 30, color = "grey50") +
  scale_fill_gradient(low = "white", high = "darkred", trans = "log",
                      name = "Count") +
  labs(title = "Density of Batted Balls by Speed & Angle",
       x = "Launch Speed (mph)", y = "Launch Angle (°)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

