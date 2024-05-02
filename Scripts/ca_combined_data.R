# Clean up R memory
rm(list=ls())

# R packages
list.of.packages <- c('ggplot2', 
                      'dplyr', 
                      'ggiraphExtra', 
                      'sf', 
                      'maps', 
                      'mapproj',
                      'tigris',
                      'plotly',
                      'scales',
                      'zoo')

new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]

if(length(new.packages)) {
  install.packages(new.packages, repo='http://cran.at.r-project.org',
                   dependencies=TRUE)
}

# Load all packages
lapply(list.of.packages, library, character.only = TRUE)

#-------------------------------------------------------------------------------

# Get ca_combined_data.rds file from github if you can
ca_combined_data <- readRDS("Data/ca_combined_data.rds")

# Skip to visualizing if you loaded RDS
# Otherwise, generate a new one
# Load ca_monthly_temps.rds and ca_annual_temps.rds 
# (generated in Scripts/ca_temps_bar_line.R)
ca_monthly_temps <- readRDS("Data/ca_monthly_temps.rds")
ca_annual_temps <- readRDS("Data/ca_annual_temps.rds")
head(ca_monthly_temps, 15)
head(ca_annual_temps, 15)

# Load fires.rds
fires_df <- readRDS("Data/fires.rds")
head(fires_df)

#-------------------------------------------------------------------------------
# data manipulations (only if not loading ca_combined_data.rds)

ca_fires <- fires_df %>%
  filter(state == "CA") %>%
  group_by(year) %>%
  summarise(number_of_fires = n()/100, # scaling for visual clarity
            mean_acreage_burned = mean(size))

head(ca_fires)
head(ca_annual_temps)

# join and get moving average
ca_combined_data <- left_join(ca_annual_temps, ca_fires, by = "year")
ca_combined_data <- ca_combined_data %>%
  mutate(fires_moving_avg = rollmean(number_of_fires, k = 5, fill = NA, align = 'center'),
         acres_moving_avg = rollmean(mean_acreage_burned, k = 5, fill = NA, align = 'center')) %>%
  rename(temp_moving_avg = moving_avg)
  
head(ca_combined_data, 15)

#

# save it as rds file for later if needed
saveRDS(ca_combined_data, file = "Data/ca_combined_data.rds")

#-------------------------------------------------------------------------------
# Visualizing

combined_barplot <- ggplot(ca_combined_data, aes(x = year, y = number_of_fires, fill = annual_mean)) +
  geom_col(width = 1) +
  # geom_line(aes(y = temp_moving_avg, group = 1, color = "Temperature"), size = 1) +
  geom_line(aes(y = fires_moving_avg, group = 1, color = "Number of Fires"), size = 1) +
  geom_line(aes(y = acres_moving_avg, group = 1, color = "Acreage"), size = 1) +
  labs(title = "Annual Wildfires and Temperatures",
       subtitle = "California, 1992-2020",
       x = "Year",
       y = "Number of Fires (in hundreds)",
       color = "Moving Averages") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "grey", color = "grey"),
        panel.background = element_rect(fill = "grey", color = "grey"),
        text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12)) +
  coord_cartesian(ylim = c(45, 175)) +
  scale_fill_gradient2(low = "yellow",
                       mid = "orange",
                       high = "red",
                       midpoint = mean(ca_combined_data$annual_mean, na.rm = TRUE),
                       limits = c(min(ca_combined_data$annual_mean, na.rm = TRUE),
                                  max(ca_combined_data$annual_mean, na.rm = TRUE)),
                       name = "Mean Temperature (°F)") +
  scale_color_manual(values = c("Number of Fires" = "darkblue",
                                "Acreage" = "darkgreen"),
                                # "Temperature" = "darkred"
                     )

print(combined_barplot)





