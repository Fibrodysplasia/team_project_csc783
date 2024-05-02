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

# Load fires_df.rds, generate it with sql_to_df.R in Scripts
fires_df <- readRDS("../raw_data/fires_df.rds")
head(fires_df)

# Load California temps (Base Period: 1901-2000)
ca_temps_df <- as.data.frame(read.csv("Data/ca_temps_monthly.csv"))
head(ca_temps_df)

#-------------------------------------------------------------------------------
# Making data manipulations:

# Calculate moving averages
ca_temps_df$temp_mv_avg <- rollmean(ca_temps_df$Value, 
                                       12, 
                                       fill = NA,
                                       align = "center")

# Convert these to the first day of the given month for accurate representation
ca_temps_df$Date <- as.Date(paste0(substr(ca_temps_df$Date, 1, 4), "-", 
                                   substr(ca_temps_df$Date, 5, 6), "-01"))

# Get year
ca_temps_df$year <- format(ca_temps_df$Date, "%Y")
# Get month
ca_temps_df$month <- format(ca_temps_df$Date, "%m")
# for some reason they are <chr>
ca_temps_df$year <- as.integer(ca_temps_df$year)
ca_temps_df$month <- as.integer(ca_temps_df$month)
str(ca_temps_df)

# for plot
overall_value_mean <- mean(monthly_temps$value, na.rm = TRUE)

# create custom labels for ggplot (otherwise it's jumbled mess of text)
# only label January of each year
ca_temps_df$label <- ifelse(ca_temps_df$month == 1, as.character(ca_temps_df$year), "")
# refactored 
monthly_temps <- ca_temps_df %>%
  mutate(
    year_month = factor(paste(year, month, sep = "-"), levels = paste(year, month, sep = "-"))) %>%
  select(
    year, 
    month, 
    year_month, 
    Value, 
    Anomaly, 
    anomaly_mv_avg, 
    temp_mv_avg,
    label
  ) %>%
  rename(
    value = Value,
    anomaly = Anomaly
  ) %>%
  arrange(year, month)

str(monthly_temps)
head(monthly_temps, 15)

annual_temps <- monthly_temps %>%
  select(year, value, label) %>%
  group_by(year) %>%
  summarise(annual_mean = mean(value, na.rm = TRUE),
            label = first(label),
            .groups = 'drop')

# Calculate moving average (didn't work in summarise())
annual_temps <- annual_temps %>%
  mutate(moving_avg = rollmean(annual_mean, k = 5, fill = NA, align = 'center'))

str(annual_temps)
head(annual_temps, 12)

#-------------------------------------------------------------------------------
# Monthly Data

# TODO
# Why do the bars go all the way to the bottom now??

monthly_barplot <- ggplot(monthly_temps, aes(x = year_month, y = value, fill = value)) +
  geom_col(width = 1) +  # width = 1 for no gaps
  geom_line(aes(y = temp_mv_avg, group = 1, color = "Moving Average"), size = 1) +
  geom_line(aes(y = overall_value_mean, group = 1, color = "Mean for Period"), size = 1) +
  labs(title = "Monthly Average Temperatures",
       subtitle = "California, 1992-2020",
       x = "Year",
       y = "Temperature (°F)",
       color = "Legend") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "grey", color = "grey"),  # Set background to grey
        panel.background = element_rect(fill = "grey", color = "grey"),  # Match panel background
        text = element_text(color = "black"),  # Change text color to black for visibility
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12)) + # Rotate x-axis labels for clarity
  scale_x_discrete(labels = monthly_temps$label) +
  coord_cartesian(ylim = c(38, 80), clip = "off") + 
  scale_fill_gradient2(low = "yellow",
                       mid = "orange",
                       high = "red",
                       midpoint = overall_value_mean,
                       limits = c(min(monthly_temps$value, na.rm = TRUE),
                                  max(monthly_temps$value, na.rm = TRUE)),
                       name = "Temperature (°F)") +
  scale_color_manual(values = c("Moving Average" = "darkred", 
                                "Mean for Period" = "darkblue"))  

print(monthly_barplot)

#-------------------------------------------------------------------------------
# Annual Data

# TODO
# Why do the labels not show up here??

annual_barplot <- ggplot(annual_temps, aes(x = year, y = annual_mean, fill = annual_mean)) +
  geom_col(width = 1) +
  geom_line(aes(y = moving_avg, group = 1, color = "Moving Average"), size = 1) +
  geom_line(aes(y = overall_value_mean, group = 1, color = "Mean for Period"), size = 1) +
  labs(title = "Annual Average Temperatures",
       subtitle = "California, 1992-2020",
       x = "Year",
       y = "Temperature (°F)",
       color = "Legend") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "grey", color = "grey"),  # Set background to grey
        panel.background = element_rect(fill = "grey", color = "grey"),  # Match panel background
        text = element_text(color = "black"),  # Change text color to black for visibility
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12)) +
  scale_x_discrete(labels = annual_temps$label) +
  coord_cartesian(ylim = c(55, 63), clip = "off") + 
  scale_fill_gradient2(low = "yellow",
                       mid = "orange",
                       high = "red",
                       midpoint = overall_value_mean,
                       limits = c(min(annual_temps$annual_mean, na.rm = TRUE),
                                  max(annual_temps$annual_mean, na.rm = TRUE)),
                       name = "Temperature (°F)") +
  scale_color_manual(values = c("Moving Average" = "darkred", 
                                "Mean for Period" = "darkblue"))

print(annual_barplot)
head(annual_temps, 15)
