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
mlb |> 
  mutate(event_group = case_when(
    events == "single" ~ "Single",
    events == "double" ~ "Double",
    events == "triple" ~ "Triple",
    events == "home_run" ~ "Home Run",
    events %in% c("field_out", "fielders_choice", "fielders_choice_out", "force_out", "sac_bunt", "sac_fly") ~ "1 Out",
    events %in% c("grounded_into_double_play", "sac_fly_double_play", "triple_play", "double_play") ~ "2+ Outs",
    events %in% c("field_error") ~ "Error"
  )) |>
  mutate(event_group= factor(event_group,levels=c("2+ Outs", "1 Out", "Error", "Single", "Double", "Triple", "Home Run"))) |> 
  ggplot(aes(x=event_group,y=launch_speed))+
  geom_boxplot(fill="steelblue")+
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


#similar but hexbin
#mutate() to create indicator function for hits
#x and y axis are divided into 40 bins (each) for launch speed and launch angle
#for each hexbin (combo) of launch speed and angle, summarizes is_hit using mean (effectively proportion of hits inside that bin)

mlb |> 
  mutate(is_hit = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0)) |> 
  ggplot(aes(x = launch_speed, y = launch_angle)) +
  stat_summary_hex(
    aes(z = is_hit), 
    fun = mean, 
    bins = 40
  ) +
  scale_fill_viridis_c(option="C", name = "Hit Rate", limits = c(0, 1)) +
  labs(
    title = "Hit Rate by Launch Speed & Angle",
    x = "Launch Speed (mph)", y = "Launch Angle (°)"
  ) +
  theme_minimal()


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
  filter(!is.na(launch_angle), !is.na(events)) |> 
  ggplot(aes(x=launch_angle, y=events, fill=events))+
  geom_density_ridges(alpha=0.6)+
  theme_minimal()+
  labs(title="launch angle Distribution by Batted Ball Outcome",
       x="lan angle", y="Event")+
  theme(legend.position = "none")

cor(mlb$launch_speed, mlb$release_speed, use="complete.obs")

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

mlb |> 
  filter(events=="double") |> 
  group_by(batter_name) |> 
  count()


?reorder_within()
#top batters by event type (hits only)
library(tidytext)
mlb |> 
  filter(events%in% c("double", "single", "triple", "home_run")) |> 
  count(batter_name, events, sort=TRUE) |> 
  group_by(events) |> 
  slice_max(n, n=5) |> 
  ggplot(aes(x=reorder_within(batter_name, n, events), y=n, fill=events))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~events, scales ="free")+
  coord_flip()+
  scale_x_reordered()+
  scale_y_continuous(expand=c(0,0))+
  labs(title="Top 5 Batters by Event Type", x="Batter", y="Count")+
  theme_minimal()

#average launch speed by batter
mlb |> 
  group_by(batter_name) |> 
  summarize(avg_speed=mean(launch_speed)) |> 
  slice_max(avg_speed, n=10) |> 
  ggplot(aes(x=reorder(batter_name, avg_speed), y=avg_speed))+
  geom_col(fill="red")+
  coord_flip()+
  labs(title="Top 10 Batters by Avg Launch Speed", x="Batter", y="Avg Launch Speed (mph)")+
  theme_minimal()

#spray chart for a player
mlb |> 
  filter(batter_name=="Ohtani, Shohei") |> 
  ggplot(aes(x=hit_coord_x, y=-hit_coord_y, color=events))+
  geom_point(alpha=0.7)+
  coord_fixed()+
  theme_minimal()+
  labs(title="Spray Chart: Shohei Ohtani", x="Horizontal Field Location", y="Vertical Field Location")


#Most common pitch types thrown in the dataset
mlb |> 
  count(pitch_name, sort=TRUE) |> 
  slice_max(n, n=10) |> 
  ggplot(aes(x=reorder(pitch_name, n),y=n))+
  geom_col(fill="red")+
  coord_flip()+
  labs(title="Top 10 Most Common Pitch Types", x="Pitch Type", y="Count")+
  theme_minimal()


#bar plot of batted ball outcomes
mlb |> 
  count(events) |> 
  mutate(events=reorder(events, n)) |> 
  ggplot(aes(x=events, y=log(n)))+
  geom_col(fill="red")+
  coord_flip()+
  labs(title="Distribution of Batted Ball Outcomes", x="Outcome", y="Log Count")+
  theme_minimal()

#strike zone heatmap
#where in the strike zone are balls hit most often
mlb |> 
  filter(!is.na(plate_x), !is.na(plate_z)) |> 
  ggplot(aes(x=plate_x, y=plate_z))+
  stat_bin2d(bins=30)+
  scale_fill_viridis_c()+
  theme_minimal()

#most frequent batters bar chart
#which appear most in this dataset
mlb |> 
  count(batter_name, sort=TRUE) |> 
  slice_max(n,n=10) |> 
  ggplot(aes(x=reorder(batter_name, n), y=n))+
  geom_col(fill="steelblue")+
  coord_flip()+labs(title="Top 10 Most Frequent Batters",
                    x="Batter",
                    y="Number of Appearances")+
  theme_minimal()

#outcome proportions by pitch type
#how do outcomes vary by pitch type
#are certain pitches more likely to lead to good or bad outcomes?
library(viridis)
mlb |> 
  filter(!is.na(pitch_name), !is.na(events)) |> 
  count(pitch_name, events) |> 
  group_by(pitch_name) |> 
  mutate(prop=n/sum(n)) |> 
  ggplot(aes(x=pitch_name, y=prop, fill=events))+
  geom_col()+
  coord_flip()+
  theme_minimal()+
  scale_fill_viridis(discrete = TRUE)

#when are hits hardest or farthest?
mlb |> 
  group_by(inning) |> 
  summarize(avg_exit_velocity=mean(launch_speed, na.rm=TRUE)) |> 
  ggplot(aes(x=factor(inning), y=avg_exit_velocity))+
  geom_col(fill="steelblue")+
  theme_minimal()+
  labs(title = "Average Exit Velocity by Inning",
       x = "Inning",
       y = "Avg Exit Velocity (mph)") 

library(sportyR)
mlb |> 
  mutate(location_x = 2.5*(hit_coord_x - 125.42),
         location_y = 2.5*(198.27 - hit_coord_y)) |> 
  geom_baseball(league = "MLB")+
  geom_point(aes(locaton_x, location_y))

####################################
#KMEANS CLUSTERING
set.seed(42)
mlb_features <- mlb |>
  group_by(batter_name) |> 
  summarize(
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
  kmeans(centers=4, nstart=100)



#to visualize results with many dimensions (features), do PCA and plot
#plot top 2 dimensions (capture most of the variation in the data)
library(factoextra)

kmeans_result |> 
  fviz_cluster(data=std_mlb_features,
               geom="point",
               ellipse=TRUE)+
  ggthemes::scale_color_colorblind()+
  theme_minimal()

pca_result <- prcomp(std_mlb_features, center=TRUE, scale.=TRUE)
summary(pca_result)
pca_result$rotation


#elbow plot
library(factoextra)
std_mlb_features |> 
  fviz_nbclust(kmeans, method = "wss")

clustered_batters <- data.frame(
  batter_name = mlb_features$batter_name,
  cluster=kmeans_result$cluster
) |> 


mlb_features_clustered <- mlb |> 
  left_join(clustered_batters, by="batter_name") |> 
  filter(!is.na(cluster))


#avg bip value per cluster
mlb_features_clustered |> 
  group_by(cluster) |> 
  summarize(
    total_bip_value = sum(case_when(
      events == "single" ~ 1,
      events == "double" ~ 2,
      events == "triple" ~ 3,
      events == "home_run" ~ 4,
      TRUE ~ 0
    ), na.rm = TRUE),
    total_batted_balls = n(),  
    avg_bip_value = total_bip_value / total_batted_balls
  ) |> 
  ggplot(aes(x=factor(cluster), y=avg_bip_value))+
  geom_col(fill = "red") +
  theme_minimal()
  




#Avg launch speed per cluster
mlb_features_clustered_subset <- mlb_features |> 
  left_join(clustered_batters, by="batter_name") |> 
  filter(!is.na(cluster))

mlb_features_clustered_subset |> 
  ggplot(aes(x=avg_launch_speed, y=avg_launch_angle, color=factor(cluster)))+
  geom_point(alpha=0.8)+
  ggthemes::scale_color_colorblind()+
  theme_minimal()

mlb_features_clustered_subset |> 
  group_by(cluster) |> 
  summarize(
    avg_launch_speed = mean(avg_launch_speed),
    avg_launch_angle = mean(avg_launch_angle),
    avg_bat_speed = mean(avg_bat_speed),
    number_of_batters = n()
  )


cluster_hits <- mlb_features_clustered |> 
  filter(events %in% c("single", "double", "triple", "home_run")) |> 
  group_by(cluster, events) |> 
  summarize(count = n(), .groups = "drop") 

cluster_hits |> 
  pivot_wider(
    names_from = events, 
    values_from = count, 
    values_fill = 0
  ) |> 
  select(cluster, single, double, triple, home_run)


cluster_hits |> 
  mutate(events = factor(events, levels = c("single", "double", "triple", "home_run"))) |> 
  ggplot(aes(x = factor(cluster), y = count, fill = events)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Cluster", y = "Count", fill = "Event", title = "Hit Types by Cluster") +
  theme_minimal()

#Launch Speed
mlb_features_clustered_subset |>
  group_by(cluster) |>
  summarize(avg = mean(avg_launch_speed)) |>
  ggplot(aes(x = factor(cluster), y = avg)) +
  geom_col(fill = "red") +
  labs(title = "Avg Launch Speed by Cluster", x = "Cluster", y = "mph") +
  theme_minimal()

#launch Angle
mlb_features_clustered_subset |>
  group_by(cluster) |>
  summarize(avg = mean(avg_launch_angle)) |>
  ggplot(aes(x = factor(cluster), y = avg)) +
  geom_col(fill = "blue") +
  labs(title = "Avg Launch Angle by Cluster", x = "Cluster", y = "Degrees") +
  theme_minimal()

#bat Speed
mlb_features_clustered_subset |>
  group_by(cluster) |>
  summarize(avg = mean(avg_bat_speed)) |>
  ggplot(aes(x = factor(cluster), y = avg)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Avg Bat Speed by Cluster", x = "Cluster", y = "mph") +
  theme_minimal()



