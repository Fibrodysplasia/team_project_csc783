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

# Load ca_monthly_temps.rds and ca_annual_temps.rds
ca_monthly_temps <- readRDS("Data/ca_monthly_temps.rds")
ca_annual_temps <- readRDS("Data/ca_annual_temps.rds")
head(ca_monthly_temps, 15)
head(ca_annual_temps, 15)

# Load fires.rds
fires_df <- readRDS("Data/fires.rds")
head(fires_df)

#-------------------------------------------------------------------------------
# data manipulatinos

ca_fires <- fires_df %>%
  filter(state == "CA") %>%
  group_by(year) %>%
  summarise(number_of_fires = n(),
            mean_acreage_burned = mean(size))

head(ca_fires)
head(ca_annual_temps)

# join
joined_data <- left_join(ca_annual_temps, ca_fires, by = "year")

head(joined_data, 15)

#-------------------------------------------------------------------------------






