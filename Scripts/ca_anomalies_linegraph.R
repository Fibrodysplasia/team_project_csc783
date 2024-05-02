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

# Calculate a moving average
ca_temps_df$anomaly_mv_avg <- rollmean(ca_temps_df$Anomaly, 
                                   12, 
                                   fill = NA,
                                   align = "center")

# Convert these to the first day of the given month for accurate representation
ca_temps_df$Date <- as.Date(paste0(substr(ca_temps_df$Date, 1, 4), "-", 
                                   substr(ca_temps_df$Date, 5, 6), "-01"))
# Get month
ca_temps_df$Month <- format(ca_temps_df$Date, "%m")
# Get year
ca_temps_df$Year <- format(ca_temps_df$Date, "%Y")


head(ca_temps_df, 50)
#-------------------------------------------------------------------------------
# Anomalies over time

head(ca_temps_df)

ca_anomalies_lineplot<- ggplot(ca_temps_df, aes(x = Date, y = Anomaly)) +
  geom_line(color = "black") +  # Adding a custom color to the line
  geom_point(aes(color = Anomaly), size = 2, alpha = 0.6) +  # Adding points
  geom_line(aes(y = anomaly_mv_avg), color = "darkred", size = 1.5) + # Anomaly moving Average
  scale_color_gradientn(colors = c("blue", "yellow", "red")) +  # Color gradient for points
  labs(
    title = "Temperature Anomalies in California",
    subtitle = "Monthly anomalies relative to the 1901-2000 average",
    x = "Year",
    y = "Anomaly (°F)",
    caption = "Data source: NOAA"
  ) +
  theme_minimal() +  # Minimalist theme
  theme(
    plot.background = element_rect(fill = "grey", color = "grey"),  # Set background to grey
    panel.background = element_rect(fill = "grey", color = "grey"),  # Match panel background
    text = element_text(color = "black"),  # Change text color to black for visibility
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12)
  ) +
  guides(color = guide_legend(title = "Temperature Anomaly"))

ca_anomalies_lineplot

# Convert the ggplot object to a plotly object
ca_anomalies_plotly <- ggplotly(ca_anomalies_lineplot, tooltip = c("x", "y", "color"))

# Display the interactive plot
ca_anomalies_plotly

# Range Slider addition
ca_anomalies_plotly <- layout(ca_anomalies_plotly,
                        xaxis = list(
                          tickformat = "%Y-%m",
                          rangeslider = list(type = "date"),
                          rangeselector = list(
                            buttons = list(
                              list(count = 1, label = "1m", step = "month", stepmode = "backward"),
                              list(count = 6, label = "6m", step = "month", stepmode = "backward"),
                              list(count = 1, label = "1y", step = "year", stepmode = "backward"),
                              list(step = "all")
                            )
                          )
                        ),
                        yaxis = list(
                          fixedrange = FALSE  # Allows zooming on the y-axis as well
                        ))
ca_anomalies_plotly

#-------------------------------------------------------------------------------
