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
# TODO
# I broke the moving averages somehow when adding anomaly data. Need to fix
#-------------------------------------------------------------------------------
# Making data manipulations:

# Calculate a moving average
ca_temps_df$moving_avg <- rollmean(ca_temps_df$Anomaly, 
                                   12, 
                                   fill = NA,
                                   align = "center")
# Get month
ca_temps_df$Month <- format(ca_temps_df$Date, "%m")
# Get year
ca_temps_df$Year <- format(ca_temps_df$Date, "%Y")


head(ca_temps_df, 50)
#-------------------------------------------------------------------------------
# Anomalies over time

# Convert these to the first day of the given month for accurate representation
ca_temps_df$Date <- as.Date(paste0(substr(ca_temps_df$Date, 1, 4), "-", 
                                   substr(ca_temps_df$Date, 5, 6), "-01"))
head(ca_temps_df)

ca_anomalies_lineplot<- ggplot(ca_temps_df, aes(x = Date, y = Anomaly)) +
  geom_line(color = "black") +  # Adding a custom color to the line
  geom_point(aes(color = Anomaly), size = 2, alpha = 0.6) +  # Adding points
  geom_line(aes(y = moving_avg), color = "darkred", size = 1.5) + # Moving Average
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
    plot.background = element_rect(fill = "grey", color = "grey"),  # Set background to black
    panel.background = element_rect(fill = "grey", color = "grey"),  # Match panel background
    text = element_text(color = "black"),  # Change text color to white for visibility
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


#-------------------------------------------------------------------------------
# Get the columns of interest and rename them
fires <- fires_df %>%
  select(FIRE_YEAR,
         STATE,
         COUNTY,
         DISCOVERY_DATE,
         CONT_DATE,
         FIRE_SIZE_CLASS,
         FIRE_SIZE,
         NWCG_CAUSE_CLASSIFICATION) %>%
  rename(year = FIRE_YEAR,
         state = STATE,
         county = COUNTY,
         day_discovered = DISCOVERY_DATE,
         day_contained = CONT_DATE,
         class = FIRE_SIZE_CLASS,
         size = FIRE_SIZE,
         cause = NWCG_CAUSE_CLASSIFICATION)

head(fires)

# stateR was not working and this was a faster fix than finding another library
state_abbreviations <- c(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR", "California" = "CA",
  "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE", "Florida" = "FL", "Georgia" = "GA",
  "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA",
  "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
  "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS", "Missouri" = "MO",
  "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV", "New Hampshire" = "NH", "New Jersey" = "NJ",
  "New Mexico" = "NM", "New York" = "NY", "North Carolina" = "NC", "North Dakota" = "ND",
  "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI",
  "South Carolina" = "SC", "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX",
  "Utah" = "UT", "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV",
  "Wisconsin" = "WI", "Wyoming" = "WY")

#----example map----------------------------------------------------------------
options(tigris_class = "sf")  # Ensure it returns sf objects
# cb = TRUE gives us a low-res version for quicker plotting
states_sf <- tigris::states(cb = TRUE)
states_sf <- states_sf %>%
  mutate(NAME = state_abbreviations[NAME])
states_sf <- states_sf %>%
  filter(!is.na(NAME))
# ensure you have the same identifiers
state_total_acreage$state <- trimws(state_total_acreage$state)
print(unique(states_sf$NAME))
print(unique(state_total_acreage$state))

# Merge using the common 'state' abbreviation column
choropleth_data <- merge(states_sf, 
                         state_total_acreage, 
                         by.x = "NAME", 
                         by.y = "state", 
                         all.x = TRUE)

# Check the result after merging, shouldn't be NA
summary(choropleth_data$tot_acreage)

# # Filter
choropleth_data_no_ak_hi <- choropleth_data %>%
  filter(!NAME %in% c("AK", "HI"))  # Exclude both Alaska and Hawaii

ggplot_map_no_ak_hi <- ggplot(data = choropleth_data_no_ak_hi) +
  geom_sf(aes(fill = tot_acreage), 
          color = "white", 
          size = 0.1) +
  scale_fill_gradient(low = "yellow", 
                      high = "red", 
                      name = "Total Burned Acreage",
                      labels = scales::comma) +  # Adjust scale labels to standard numbering
  labs(title = "Total Burned Acreage by State (excluding Alaska & Hawaii)",
       subtitle = "Darker colors indicate more acreage burned. 1992-2020",
       x = NULL, # Remove axis titles
       y = NULL) +  # They are just coordinate labels
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove X axis text
        axis.text.y = element_blank(),  # Remove Y axis text
        axis.ticks = element_blank(),   # Remove axis ticks
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.background = element_blank()) +
  coord_sf(xlim = c(-125, -66), 
           ylim = c(24, 50), 
           expand = FALSE)

ggplot_map_no_ak_hi

# ------------------------------------------------------------------------------




