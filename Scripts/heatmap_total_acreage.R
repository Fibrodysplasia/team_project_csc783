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
                      'scales')

new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]

if(length(new.packages)) {
  install.packages(new.packages, repo='http://cran.at.r-project.org',
                   dependencies=TRUE)
}

# Load all packages
lapply(list.of.packages, library, character.only = TRUE)

# ----------------------------------------------------------
# Load fires_df.rds, generate it with sql_to_df.R in Scripts
fires_df <- readRDS("Data/fires_df.rds")

# Get the columns of interest and rename them
fires <- fires_df %>%
  select(FIRE_YEAR,
         STATE,
         COUNTY,
         DISCOVERY_DOY,
         CONT_DOY,
         FIRE_SIZE_CLASS,
         FIRE_SIZE,
         NWCG_CAUSE_CLASSIFICATION) %>%
  rename(year = FIRE_YEAR,
         state = STATE,
         county = COUNTY,
         day_discovered = DISCOVERY_DOY,
         day_contained = CONT_DOY,
         class = FIRE_SIZE_CLASS,
         size = FIRE_SIZE,
         cause = NWCG_CAUSE_CLASSIFICATION)

state_total_acreage <- fires %>%
  mutate(state <- tolower(state)) %>%
  group_by(state) %>%
  summarise(tot_acreage = sum(size))


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


# ----------------- Choropleth Map: Total Acreage Burned -----------------------
# Initial map
# ggplot_map <- ggplot(data = choropleth_data) +
#               geom_sf(aes(fill = tot_acreage),
#                       color = "white",
#                       size = 0.1) +
#               scale_fill_gradient(low = "yellow",
#                                   high = "red",
#                                   name = "Total Burned Acreage") +
#               labs(title = "Total Burned Acreage by State, 1992-2022",
#                    subtitle = "Darker colors indicate more acreage burned") +
#               theme_minimal() +
#               coord_sf(xlim = c(-170, -66),
#                        ylim = c(15, 72),
#                        expand = FALSE)
# ggplot_map
# excludes Alaska and Hawaii ---------------------------------------------------
# (often necessary to focus visualization on the continental United States 
# due to Alaska's geographical and data scale differences)----------------------

# # Filter
# choropleth_data_no_ak_hi <- choropleth_data %>%
#   filter(!NAME %in% c("AK", "HI"))  # Exclude both Alaska and Hawaii
# 
# # Create the choropleth map
# ggplot_map_no_ak_hi <- ggplot(data = choropleth_data_no_ak_hi) +
#   geom_sf(aes(fill = tot_acreage),
#           color = "white",
#           size = 0.1) +
#   scale_fill_gradient(low = "yellow",
#                       high = "red",
#                       name = "Total Burned Acreage") +
#   labs(title = "Total Burned Acreage by State (excluding Alaska & Hawaii)",
#        subtitle = "Darker colors indicate more acreage burned. 1992-2020") +
#   theme_minimal() +
#   coord_sf(xlim = c(-125, -66),
#            ylim = c(24, 50),
#            expand = FALSE)
# 
# ggplot_map_no_ak_hi

# A cleaned up version
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




