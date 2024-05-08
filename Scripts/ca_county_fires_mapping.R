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
                      'zoo',
                      'corrplot',
                      'kable')

new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]

if(length(new.packages)) {
  install.packages(new.packages, repo='http://cran.at.r-project.org',
                   dependencies=TRUE)
}

# Load all packages
lapply(list.of.packages, library, character.only = TRUE)

#-------------------------------------------------------------------------------
# Load fires_df.rds
fires_df <- readRDS("Data/fires_df.rds")
View(fires_df)

ca_counties <- fires_df %>%
  group_by(county) %>%
  summarise(tot_acreage = sum(FIRE_SIZE),
            tot_human = sum(NWCG_CAUSE_CLASSIFICATION == "Human"),
            tot_natural = sum(NWCG_CAUSE_CLASSIFICATION == "Natural"),
            pct_human = 100 * (tot_human/(tot_human+tot_natural)))
str(fires_df)

ca_counties # this is broken, check the county column to see why
# need to remake from fires_df and clean up the data before this will work

#-------------------------------------------------------------------------------
# County Mapping

options(tigris_use_cache = TRUE)
counties <- counties(state = "CA", class = "sf")

counties <- counties %>%
  rename(county = COUNTYFP)
View(counties)
View(ca_counties)

ca_county_fires <- left_join(ca_counties, counties, by = "county")

ggplot_counties <- ggplot(data = ca_counties) +
  geom_sf(aes(fill = tot_acreage),
          color = "white",
          size = 0.2) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "California Counties",
       fill = "Total Acreage Burned") +
  theme_minimal()

print(ggplot_counties)
